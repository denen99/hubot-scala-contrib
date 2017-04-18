package com.iheart.listeners

import com.amazonaws.auth.{AWSStaticCredentialsProvider, BasicAWSCredentials}
import com.amazonaws.regions.Regions
import com.amazonaws.services.ec2._
import com.amazonaws.services.ec2.model._
import com.amazonaws.services.route53.AmazonRoute53ClientBuilder
import com.amazonaws.services.route53.model.{ListResourceRecordSetsRequest, ListResourceRecordSetsResult, ResourceRecordSet}
import com.amazonaws.services.elasticloadbalancing.AmazonElasticLoadBalancingClientBuilder
import com.amazonaws.services.elasticloadbalancing.model.DescribeLoadBalancersRequest
import com.typesafe.config.Config

import scala.collection.JavaConverters._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import org.dberg.hubot.utils.Helpers._

import scala.annotation.tailrec


trait AmazonHelpers {

  /** ******************************************
    * Get access_key from configuration file
    * *******************************************/
   def getAccessKey(env: String, service: String = "ec2") =
    getConfString(s"listeners.${service}.${env}.aws_access_key", s"listeners.prod.ec2.aws_access_key")

  /** ******************************************
    * Get access_secret_key from configuration file
    * *******************************************/
   def getSecretKey(env: String, service: String = "ec2") =
    getConfString(s"listeners.${service}.${env}.aws_secret_key", s"listeners.prod.ec2.aws_secret_key")

  /*
   EC2 Helpers
   */

  object EC2 {
    /** ******************************************
      * Extract the "Name" tag from the list of tags
      * *******************************************/
     def getNameFromTag(tags: List[Tag], tagName: String) = {
      tags.filter(t => t.getKey == "Name").map(t => t.getValue) match {
        case Nil => "[NoName]"
        case x => x.head
      }
    }

    /** ******************************************
      * See if this instance has the tag we need
      * *******************************************/
     def instanceHasTag(tags: List[Tag], tagName: String): Boolean =
      tags.filter(t => t.getKey == tagName).map(t => t.getValue).nonEmpty

    /** ******************************************
      * Get VPC ID from the instance
      * *******************************************/
     def instanceInVpc(i: Instance, vpc: String) =
      i.getVpcId == vpc

    /** ******************************************
      * Extract the Tags from the instance
      * *******************************************/
     def getTagsFromInstance(i: Instance) =
      i.getTags.iterator().asScala.toList

    /** ******************************************
      * Get VPC ID from configuration file
      * *******************************************/
     def getVpcFromEnv(str: String) =
      getConfString("listeners.ec2." + str + ".vpc_id", "listeners.ec2.prod.vpc_id")



    /** ******************************************
      * Generic function to return all instances for
      * an environment that have a tag that matches
      * the regex string
      * *******************************************/
     def getInstancesBy(regex: String, tagName: String, env: String) = {
      val creds = new BasicAWSCredentials(getAccessKey(env), getSecretKey(env))
      val ec2Client = AmazonEC2ClientBuilder.standard()
        .withCredentials(new AWSStaticCredentialsProvider(creds))
        .withRegion(Regions.US_EAST_1)
        .build()

      val request = new DescribeInstancesRequest
      val f1 = new Filter("tag:" + tagName, List(regex).asJava)
      request.withFilters(List(f1).asJava)

      val resp = ec2Client.describeInstances(request)

      resp.getReservations.asScala.toList.flatMap { res =>
        res.getInstances.asScala.toList.filter { i =>
          instanceHasTag(getTagsFromInstance(i), tagName) && instanceInVpc(i, getVpcFromEnv(env))
        }.map { i2 =>
          getNameFromTag(getTagsFromInstance(i2), tagName)
        }
      }
    }

  }

  /*
    Route53 Helpers
   */

  object Route53 {

    /** ******************************************
      * Get ZoneID from configuration file
      * *******************************************/
     def getZoneIdFromEnv(env: String) =
      getConfString("listeners.route53." + env + ".zone_id", "listeners.route53.prod.zone_id")

  }

  /*
    ELB Helpers
   */

  object ELB {
    /** ******************************************
      * Get private IPs from the given Instance ID
      * *******************************************/
    def getPrivateIpFromId(id: String, env: String): Seq[String] = {
      val creds = new BasicAWSCredentials(getAccessKey(env, "ec2"), getSecretKey(env, "ec2"))

      val ec2Client = AmazonEC2ClientBuilder.standard()
        .withCredentials(new AWSStaticCredentialsProvider(creds))
        .withRegion(Regions.US_EAST_1)
        .build()
      val request = new DescribeInstancesRequest
      request.withInstanceIds(Seq(id).asJava)

      val resp = ec2Client.describeInstances(request)
      resp.getReservations.asScala.toList.flatMap { r =>
        r.getInstances.asScala.toList.map { i =>
          i.getPrivateIpAddress
        }
      }
    }

  }


  /*
    Public method for querying EC2 instances
   */

  /********************************************
   Public method to query EC2 instances
    ********************************************/
  def getInstances(regex: String, tags: List[String], env: String): List[String] = {
    val fut = Future.sequence(tags.map(t => Future { EC2.getInstancesBy(regex,t,env)}))
    val results = Await.result(fut,30.seconds)
    results.flatten.distinct.sorted
  }


  /*
   Public method for querying Route53
  */
  /********************************************
   Get DNS records that match regex

   This one is a little more annoying b/c Route53 only returns
   100 records at a time, so you need to keep looping until you
   are done
    ********************************************/
  def fetchRecords(regex: String, env: String): Seq[(String,String,String)] = {

    val creds = new BasicAWSCredentials(getAccessKey(env,"route53"),getSecretKey(env,"route53"))

    val zoneId = Route53.getZoneIdFromEnv(env)

    val client = AmazonRoute53ClientBuilder.standard()
      .withCredentials(new AWSStaticCredentialsProvider(creds))
      .withRegion(Regions.US_EAST_1)
      .build()

    val req = new ListResourceRecordSetsRequest(zoneId)

    @tailrec
    def fetchEntries(result: ListResourceRecordSetsResult, entries: Seq[(String,String,String)] = Seq()): Seq[(String,String,String)]  = result.getNextRecordName match {

      case _: String =>
        val sets = result.getResourceRecordSets.asScala.toList
        req.setStartRecordName(result.getNextRecordName)
        fetchEntries(client.listResourceRecordSets(req),entries ++ sets.flatMap(s => s.getResourceRecords.asScala.toList.map(r => (s.getName,s.getType,r.getValue))))
      case _ =>
        entries
    }

    val rs = client.listResourceRecordSets(req)
    val entries = fetchEntries(rs)
    entries.filter(e => e._1.contains(regex))

  }

  /*
  Public Methods for Querying ELB
   */

  /********************************************
   Return info for a single ELB name
    ********************************************/
  def getElbInfo(name: String, env: String): List[Map[String,String]] = {
    val creds = new BasicAWSCredentials(getAccessKey(env),getSecretKey(env))

    val elbClient = AmazonElasticLoadBalancingClientBuilder.standard()
      .withCredentials(new AWSStaticCredentialsProvider(creds))
      .withRegion(Regions.US_EAST_1)
      .build()

    val req = new DescribeLoadBalancersRequest()
    req.withLoadBalancerNames(Seq(name).asJava)

    val lb = elbClient.describeLoadBalancers(req)
    lb.getLoadBalancerDescriptions.asScala.toList.map { x =>
      val ports = x.getListenerDescriptions.asScala.toList.map(p => p.getListener.getInstanceProtocol + ":" + p.getListener.getInstancePort)
      val instances: List[String] = x.getInstances.asScala.toList.flatMap(i => ELB.getPrivateIpFromId(i.getInstanceId,env))
      val health = x.getHealthCheck.getTarget
      Map("ports" -> ports.mkString(","), "instances" -> instances.mkString(","), "healthcheck" -> health)
    }
  }

  /********************************************
   Return all ELBs that match regex
    ********************************************/
  def queryELbs(regex: String, env: String): List[(String,String)] = {

    val creds = new BasicAWSCredentials(getAccessKey(env),getSecretKey(env))

    val elbClient = AmazonElasticLoadBalancingClientBuilder.standard()
      .withCredentials(new AWSStaticCredentialsProvider(creds))
      .withRegion(Regions.US_EAST_1)
      .build()

    val lbs = elbClient.describeLoadBalancers()

    lbs.getLoadBalancerDescriptions.asScala.toList.filter(l => l.getLoadBalancerName.contains(regex))
      .map { x =>
        val lbName: String = x.getLoadBalancerName
        val dns: String = x.getDNSName
        (lbName,dns)
      }
  }

}

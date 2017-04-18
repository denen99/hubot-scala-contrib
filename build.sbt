val awsVersion = "1.11.109"

def awsSdkModule(id: String) = "com.amazonaws" % s"aws-java-sdk-$id" % awsVersion

lazy val root = (project in file(".")).
  settings(
    name := "hubot-scala-contrib",
    version := "0.0.1",
    scalaVersion := "2.11.8",
    retrieveManaged := true,
    libraryDependencies ++= Seq(
       "org.dberg" % "hubot-scala_2.11" % "0.2.5"
    ) ++ Seq("ec2","elasticloadbalancing","s3", "route53").map(awsSdkModule)
)


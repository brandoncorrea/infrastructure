(ns inf.ec2-spec
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.env :as env]
            [c3kit.apron.time :as time]
            [inf.ec2 :as sut]
            [speclj.core :refer :all])
  (:import (java.util List)
           (software.amazon.awssdk.auth.credentials AwsCredentialsProvider)
           (software.amazon.awssdk.regions Region)
           (software.amazon.awssdk.services.ec2 DefaultEc2ClientBuilder Ec2Client Ec2ClientBuilder)
           (software.amazon.awssdk.services.ec2.model DescribeImagesResponse DescribeInstancesResponse DescribeKeyPairsResponse DescribeSecurityGroupsResponse GroupIdentifier Image Instance InstanceState InstanceStateChange InstanceType IpPermission IpRange KeyPairInfo Reservation RunInstancesResponse SecurityGroup TerminateInstancesRequest TerminateInstancesResponse)))

(defn ->ip-range [cidr-ip]
  (-> (IpRange/builder)
      (.cidrIp cidr-ip)
      .build))

(defn ->ip-permission [from to protocol cidr-ips]
  (-> (IpPermission/builder)
      (.fromPort (int from))
      (.toPort (int to))
      (.ipProtocol protocol)
      (.ipRanges ^List (map ->ip-range cidr-ips))
      .build))

(defn ^InstanceState ->state [^String name]
  (-> (InstanceState/builder)
      (.name name)
      .build))

(defn ->utc-instant [year month day hour minute second]
  (.toInstant (time/utc year month day hour minute second)))

(def empty-instance (.build (Instance/builder)))
(def running-instance
  (-> (Instance/builder)
      (.instanceId "instance-1")
      (.imageId "image-1")
      (.launchTime (->utc-instant 2022 1 2 11 12 13))
      (.privateDnsName "private dns")
      (.privateIpAddress "private ip")
      (.publicDnsName "public dns")
      (.publicIpAddress "public ip")
      (.instanceType InstanceType/T2_SMALL)
      (.state (->state "running"))
      (.tags [(sut/->tag "tag-key-1" "tag-value-1")
              (sut/->tag "tag-key-2" "tag-value-2")])
      .build))

(def running-map
  {:id          "instance-1"
   :image       "image-1"
   :launched-at (time/utc 2022 1 2 11 12 13)
   :private-dns "private dns"
   :private-ip  "private ip"
   :public-dns  "public dns"
   :public-ip   "public ip"
   :type        :t2.small
   :state       :running
   :tags        #{"tag-value-1" "tag-value-2"}})

(def empty-group (.build (SecurityGroup/builder)))
(def group-1
  (-> (SecurityGroup/builder)
      (.groupId "group-1")
      (.vpcId "vpc-1")
      (.ownerId "owner-id")
      (.groupName "Group One")
      (.description "the first group")
      (.ipPermissions [(->ip-permission 22 23 "tcp" [])
                       (->ip-permission 4334 4334 "udp" ["0.0.0.0/0" "1.1.1.1/1"])])
      (.tags [(sut/->tag "tag-1-key" "tag-1-val")
              (sut/->tag "tag-2-key" "tag-2-val")])
      .build))

(def group-1-map
  {:id             "group-1"
   :vpc            "vpc-1"
   :owner          "owner-id"
   :name           "Group One"
   :description    "the first group"
   :ip-permissions [{:from 22 :to 23 :protocol "tcp"}
                    {:from 4334 :to 4334 :protocol "udp" :ranges ["0.0.0.0/0" "1.1.1.1/1"]}]
   :tags           #{"tag-1-val" "tag-2-val"}})

(defn captured-builder []
  (let [opts (atom {})]
    (proxy [Ec2ClientBuilder] []
      (region [region]
        (swap! opts assoc :region region)
        this)
      (credentialsProvider [provider]
        (swap! opts assoc :credentials-provider provider)
        this)
      (build [] @opts))))

(defmacro client-should-be [client access-key secret-key region]
  `(let [client#   ~client
         region#   (:region client#)
         provider# (:credentials-provider client#)
         creds#    (.resolveCredentials provider#)]
     (should-be-a AwsCredentialsProvider provider#)
     (should= ~access-key (.accessKeyId creds#))
     (should= ~secret-key (.secretAccessKey creds#))
     (should= ~region region#)))

(defn ^Reservation ->reservation [^List instances]
  (-> (Reservation/builder)
      (.instances instances)
      .build))

(defn ->describe-instances-response [^List reservation-instances]
  (let [^List reservations (map ->reservation reservation-instances)]
    (-> (DescribeInstancesResponse/builder)
        (.reservations reservations)
        .build)))

(defn- ->describe-security-groups-response [^List groups]
  (-> (DescribeSecurityGroupsResponse/builder)
      (.securityGroups groups)
      (.build)))

(defn ->terminated-state-change [instance-id]
  (-> (InstanceStateChange/builder)
      (.instanceId instance-id)
      (.previousState (->state "running"))
      (.currentState (->state "terminated"))
      .build))

(def my-id "me-id")

(def empty-image
  (-> (Image/builder)
      (.ownerId my-id)
      .build))

(def clojure-image
  (-> (Image/builder)
      (.imageId "clj-id")
      (.ownerId my-id)
      (.imageType "machine")
      (.state "available")
      (.description "this is a clojure image")
      (.name "clojure")
      (.tags [(sut/->tag "java-key" "java-17")
              (sut/->tag "clj-key" "clojure-1.11.1")])
      (.creationDate "2022-01-03T13:22:09.000Z")
      .build))

(def clojure-image-map
  {:id          "clj-id"
   :type        "machine"
   :state       "available"
   :description "this is a clojure image"
   :name        "clojure"
   :tags        #{"java-17" "clojure-1.11.1"}
   :created-at  (time/utc 2022 1 3 13 22 9)})

(def not-my-image
  (-> (Image/builder)
      (.imageId "vb-id")
      (.ownerId ":(")
      (.imageType "machine")
      (.state "failed")
      (.description "this is not a self-made image")
      (.name "visual-basic")
      (.tags [(sut/->tag "net-framework" "net-4.3.7")])
      (.creationDate "2006-09-22T01:23:45.000Z")
      .build))

(defn ->describe-images-response [request images]
  (let [images (cond->> images
                        (= ["self"] (.owners request))
                        (filter #(= my-id (.ownerId %))))]
    (-> (DescribeImagesResponse/builder)
        (.images ^List images)
        .build)))

(defn ->group-id [id]
  (-> (GroupIdentifier/builder)
      (.groupId id)
      .build))

(defn instance-request->tags [request]
  (->> (.tagSpecifications request)
       (filter #(= "instance" (.resourceTypeAsString %)))
       (mapcat #(.tags %))))

(defn ->instance [request]
  (-> (Instance/builder)
      (.imageId (.imageId request))
      (.instanceType (.instanceType request))
      (.securityGroups ^List (map ->group-id (.securityGroupIds request)))
      (.tags ^List (instance-request->tags request))
      (.keyName (.keyName request))
      .build))

(defn ->run-instances-response [request]
  (assert (= 1 (.minCount request) (.maxCount request)) "min and max counts must be 1")
  (-> (RunInstancesResponse/builder)
      (.instances [(->instance request)])
      .build))

(def empty-key-pair (.build (KeyPairInfo/builder)))
(def ubuntu-key-pair
  (-> (KeyPairInfo/builder)
      (.keyPairId "ubuntu-key")
      (.keyFingerprint "the-fingerprint")
      (.keyName "Ubuntu")
      (.keyType "rsa")
      (.tags [(sut/->tag "the-key" "the-value")])
      (.publicKey "the-public-key")
      (.createTime (->utc-instant 2024 1 2 3 4 5))
      .build))

(def ubuntu-key-map
  {:id          "ubuntu-key"
   :fingerprint "the-fingerprint"
   :name        "Ubuntu"
   :type        :rsa
   :tags        #{"the-value"}
   :public-key  "the-public-key"
   :created-at  (time/utc 2024 1 2 3 4 5)})

(defn ->describe-key-pairs-response [key-pairs]
  (-> (DescribeKeyPairsResponse/builder)
      (.keyPairs ^List key-pairs)
      (.build)))

(defn ->proxied-client [& {:keys [reservations images groups key-pairs]}]
  (proxy [Ec2Client] []
    (describeInstances []
      (->describe-instances-response reservations))
    (describeKeyPairs [_request]
      (->describe-key-pairs-response key-pairs))
    (describeImages [request]
      (->describe-images-response request images))
    (describeSecurityGroups []
      (->describe-security-groups-response groups))
    (runInstances [request]
      (->run-instances-response request))
    (terminateInstances [^TerminateInstancesRequest request]
      (let [^List state-changes (map ->terminated-state-change (.instanceIds request))]
        (-> (TerminateInstancesResponse/builder)
            (.terminatingInstances state-changes)
            .build)))))

(describe "EC2"

  (context "launch"

    (it "with string type"
      (let [client   (->proxied-client)
            instance (sut/launch client {:ami      "ami-id"
                                         :type     "t2.micro"
                                         :key-name "my-key"
                                         :groups   ["sg-123" "sg-456"]})]
        (should= "ami-id" (:image instance))
        (should= :t2.micro (:type instance))
        (should= "my-key" (:key-name instance))
        (should= #{"sg-123" "sg-456"} (:groups instance))
        (should-not-contain :tags instance)))

    (it "with keyword type"
      (let [client   (->proxied-client)
            instance (sut/launch client {:ami      "ami-id"
                                         :type     :t3.medium
                                         :key-name "my-key"
                                         :groups   ["sg-123" "sg-456"]})]
        (should= "ami-id" (:image instance))
        (should= :t3.medium (:type instance))
        (should= "my-key" (:key-name instance))
        (should= #{"sg-123" "sg-456"} (:groups instance))
        (should-not-contain :tags instance)))

    (it "with tags"
      (let [client   (->proxied-client)
            instance (sut/launch client {:ami      "ami-id"
                                         :type     :t3.medium
                                         :key-name "my-key"
                                         :groups   ["sg-123" "sg-456"]
                                         :tags     {"first"  "one"
                                                    "second" "two"}})]
        (should= "ami-id" (:image instance))
        (should= :t3.medium (:type instance))
        (should= "my-key" (:key-name instance))
        (should= #{"sg-123" "sg-456"} (:groups instance))
        (should= #{"one" "two"} (:tags instance))))

    )

  (context "Client"

    (redefs-around [env/env {"AWS_ACCESS_KEY" "env-access-key"
                             "AWS_SECRET_KEY" "env-secret-key"
                             "AWS_REGION"     "us-west-1"}])

    (it "builder"
      (let [builder (sut/->client-builder)]
        (should-be-a DefaultEc2ClientBuilder builder)))

    (it "no params"
      (with-redefs [sut/->client-builder captured-builder]
        (let [client (sut/->client)]
          (client-should-be client "env-access-key" "env-secret-key" Region/US_WEST_1))))

    (it "region only"
      (with-redefs [sut/->client-builder captured-builder]
        (let [client (sut/->client "us-east-1")]
          (client-should-be client "env-access-key" "env-secret-key" Region/US_EAST_1))))

    (it "with string region"
      (with-redefs [sut/->client-builder captured-builder]
        (let [client (sut/->client "foo" "bar" :us-east-1)]
          (client-should-be client "foo" "bar" Region/US_EAST_1))))

    (it "with type Region"
      (with-redefs [sut/->client-builder captured-builder]
        (let [client (sut/->client "baz" "buzz" Region/US_WEST_2)]
          (client-should-be client "baz" "buzz" Region/US_WEST_2))))
    )

  (context "instances"

    (it "no instances"
      (let [client    (->proxied-client)
            instances (sut/instances client)]
        (should-be empty? instances)))

    (it "one empty instance"
      (let [client    (->proxied-client :reservations [[empty-instance]])
            instances (sut/instances client)]
        (should= [{}] instances)))

    (it "one running instance"
      (let [client    (->proxied-client :reservations [[running-instance]])
            instances (sut/instances client)]
        (should= [running-map] instances)))

    (it "two instances on different reservations"
      (let [client    (->proxied-client :reservations [[running-instance] [empty-instance]])
            instances (sut/instances client)]
        (should= [running-map {}] instances)))

    (it "two instances on the same reservation"
      (let [client    (->proxied-client :reservations [[empty-instance running-instance]])
            instances (sut/instances client)]
        (should= [{} running-map] instances)))
    )

  (context "security groups"

    (it "no groups"
      (let [client (->proxied-client)
            groups (sut/security-groups client)]
        (should-be empty? groups)))

    (it "one empty group"
      (let [client (->proxied-client :groups [empty-group])
            groups (sut/security-groups client)]
        (should= [{}] groups)))

    (it "one populated group"
      (let [client (->proxied-client :groups [group-1])
            groups (sut/security-groups client)]
        (should= [group-1-map] groups)))

    (it "two groups"
      (let [client (->proxied-client :groups [group-1 empty-group])
            groups (sut/security-groups client)]
        (should= [group-1-map {}] groups)))

    )

  (context "terminate"

    (it "no instances"
      (let [client    (->proxied-client)
            response  (sut/terminate client [])
            instances (.terminatingInstances response)]
        (should-be empty? instances)))

    (it "one instance"
      (let [client    (->proxied-client)
            response  (sut/terminate client ["instance-1"])
            instances (.terminatingInstances response)]
        (should= 1 (count instances))
        (should= "instance-1" (.instanceId (first instances)))))

    (it "two instances"
      (let [client    (->proxied-client)
            response  (sut/terminate client ["instance-1" "instance-2"])
            instances (.terminatingInstances response)]
        (should= 2 (count instances))
        (should= "instance-1" (.instanceId (first instances)))
        (should= "instance-2" (.instanceId (second instances)))))

    )

  (context "images"

    (it "no images"
      (let [client (->proxied-client)
            images (sut/images client)]
        (should-be empty? images)))

    (it "one empty image"
      (let [client (->proxied-client :images [empty-image])
            images (sut/images client)]
        (should= 1 (count images))
        (should= [{}] images)))

    (it "two images"
      (let [client (->proxied-client :images [clojure-image empty-image])
            images (sut/images client)]
        (should= 2 (count images))
        (should= clojure-image-map (first images))
        (should= {} (second images))))

    (it "requests own images"
      (let [client (->proxied-client :images [clojure-image empty-image not-my-image])
            images (sut/images client)]
        (should= 2 (count images))
        (should-not-contain not-my-image images)))

    )

  (context "key-pairs"

    (it "none"
      (let [client (->proxied-client)
            images (sut/key-pairs client)]
        (should-be empty? images)))

    (it "one empty key-pair"
      (let [client (->proxied-client :key-pairs [empty-key-pair])
            images (sut/key-pairs client)]
        (should= [{}] images)))

    (it "one populated key-pair"
      (let [client (->proxied-client :key-pairs [ubuntu-key-pair])
            images (sut/key-pairs client)]
        (should= [ubuntu-key-map] images)))

    (it "two key-pairs"
      (let [client (->proxied-client :key-pairs [empty-key-pair ubuntu-key-pair])
            images (sut/key-pairs client)]
        (should= [{} ubuntu-key-map] images)))

    )

  (context "real calls"

    (tags :slow)

    (with ec2 (sut/->client))
    (after (.close @ec2))

    (it "instances"
      (let [instances (sut/instances @ec2)
            live      (ccc/find-by instances :state ['not= :terminated])]
        (prn "count:" (count instances))
        (prn "live:" (count live))
        (run! prn live)))

    (it "key-pairs"
      (let [pairs (sut/key-pairs @ec2)]
        (prn "count" (count pairs))
        (run! prn pairs)))

    #_(it "terminate"
        (let [instances (sut/instances @ec2)
              found     (ccc/find-by instances :tags ["test"])]
          (sut/terminate @ec2 (map :id found))))

    (it "security groups"
      (run! prn (ccc/find-by (sut/security-groups @ec2) :name ["ssh" "web"])))

    (it "images"
      (run! prn (ccc/find-by (sut/images @ec2) :tags ["java-21" "clojure-1.11.13"])))

    #_(it "launch"
        (let [groups   (ccc/find-by (sut/security-groups @ec2) :name ["ssh" "web"])
              ami      (ccc/ffind-by (sut/images @ec2) :tags ["java-21" "clojure-1.11.13"])
              instance (sut/launch @ec2
                                   :ami (:id ami)
                                   :key-name "Macbook"
                                   :type "t2.small"
                                   :groups (map :id groups)
                                   :tags {"app" "test"
                                          "env" "staging"})]
          (prn "instance:" instance)))
    )
  )

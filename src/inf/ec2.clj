(ns inf.ec2
  (:require [c3kit.apron.corec :as ccc]
            [c3kit.apron.env :as env]
            [c3kit.apron.time :as time])
  (:import (java.util Date List)
           (software.amazon.awssdk.auth.credentials AwsBasicCredentials AwsCredentialsProvider StaticCredentialsProvider)
           (software.amazon.awssdk.regions Region)
           (software.amazon.awssdk.services.ec2 Ec2Client)
           (software.amazon.awssdk.services.ec2.model DescribeImagesRequest Instance InstanceType RunInstancesRequest Tag TagSpecification TerminateInstancesRequest)))

(defn- ^AwsCredentialsProvider ->credentials-provider [access-key secret-key]
  (-> (AwsBasicCredentials/create access-key secret-key)
      StaticCredentialsProvider/create))

(defn ->client-builder [] (Ec2Client/builder))

(defn- ->region [region]
  (if (instance? Region region)
    region
    (Region/of (name region))))

(defn ^Ec2Client ->client
  ([] (->client (env/env "AWS_REGION")))
  ([region]
   (->client (env/env "AWS_ACCESS_KEY")
             (env/env "AWS_SECRET_KEY")
             region))
  ([access-key secret-key region]
   (let [creds (->credentials-provider access-key secret-key)]
     (-> (->client-builder)
         (.region (->region region))
         (.credentialsProvider creds)
         .build))))

(defn- <-tags [tags]
  (some->> (seq tags)
           (map #(.value %))
           set))

(defn- <-instance [^Instance instance]
  (ccc/remove-nils
    {:id          (.instanceId instance)
     :image       (.imageId instance)
     :launched-at (some-> (.launchTime instance) Date/from)
     :private-dns (.privateDnsName instance)
     :private-ip  (.privateIpAddress instance)
     :public-dns  (.publicDnsName instance)
     :public-ip   (.publicIpAddress instance)
     :type        (keyword (.instanceTypeAsString instance))
     :state       (some-> (.state instance) .name str keyword)
     :tags        (<-tags (.tags instance))}))

(defn instances [client]
  (->> (.reservations (.describeInstances client))
       (mapcat #(map <-instance (.instances %)))))

(defn terminate [client instance-ids]
  (.terminateInstances
    client
    (-> (TerminateInstancesRequest/builder)
        (.instanceIds ^List instance-ids)
        .build)))

(defn- <-ip-permission [permission]
  (ccc/remove-nils
    {:from     (.fromPort permission)
     :to       (.toPort permission)
     :protocol (.ipProtocol permission)
     :ranges   (seq (map #(.cidrIp %) (.ipRanges permission)))}))

(defn- <-security-group [sg]
  (ccc/remove-nils
    {:id             (.groupId sg)
     :vpc            (.vpcId sg)
     :owner          (.ownerId sg)
     :name           (.groupName sg)
     :description    (.description sg)
     :ip-permissions (seq (map <-ip-permission (.ipPermissions sg)))
     :tags           (<-tags (.tags sg))}))

(defn security-groups [client]
  (->> (.securityGroups (.describeSecurityGroups client))
       (map <-security-group)))

(def images-request
  (-> (DescribeImagesRequest/builder)
      (.owners ["self"])
      .build))

(defn- <-image [image]
  (ccc/remove-nils
    {:id          (.imageId image)
     :type        (.imageTypeAsString image)
     :state       (.stateAsString image)
     :description (.description image)
     :name        (.name image)
     :tags        (<-tags (.tags image))
     :created-at  (some->> (.creationDate image) (time/parse "yyyy-MM-dd'T'HH:mm:ss.000'Z'"))}))

(defn images [client]
  (->> (.images (.describeImages client images-request))
       (map <-image)))

(defn ->tag
  ([[k v]] (->tag k v))
  ([k v] (-> (Tag/builder)
             (.key k)
             (.value v)
             .build)))

(defn- ->tag-specification [tags]
  (-> (TagSpecification/builder)
      (.resourceType "instance")
      (.tags ^List (map ->tag tags))
      .build))

(defn ->launch-request [{:keys [ami type key-name groups tags]}]
  (-> (RunInstancesRequest/builder)
      (.imageId ami)
      (.instanceType (InstanceType/fromValue (name type)))
      (.keyName key-name)
      (.tagSpecifications [(->tag-specification tags)])
      (.securityGroupIds ^List groups)
      (.minCount (int 1))
      (.maxCount (int 1))
      .build))

(defn launch [client & {:as options}]
  (->> (->launch-request options)
       (.runInstances client)
       .instances
       first))

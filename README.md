### aws-service-api


AWS EC2 integration layer for Cloud Haskell.


Depended on by [distributed-process-aws](http://github.com/IanConnolly/distributed-process-aws)



### Installation

* Download and extract this repository.
* Initialize a cabal sandbox ```cabal-sandbox```
* Download the patched version of [aws-sdk](https://github.com/IanConnolly/aws-sdk-fork)
* Add the patched version of aws-sdk as a source to the sandbox.
* ```cabal install```. 


### Configuration

You'll need to generate an Amazon EC2 image with libssh2-1 installed.

Fill in the ```aws.config``` file with the relevant information.

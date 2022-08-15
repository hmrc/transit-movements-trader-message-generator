
# transit-movements-trader-message-generator

Create example NCTS messages.  Contents will be valid according to the XSD files but are not guaranteed to be valid because some elements depend on other contents - see DDNA rules.

Start locally
```
sbt run
```

Example use
```
curl -X POST http://localhost:9000/messages/CC015B -o CC015B.xml
```

By default, the message is returned in XML. If you wish for Json to be returned, set the `Accept` header to `application/json`. You will need to be running `transit-movements-converter` locally to do so.
Note that only some messages can be converted into Json (specifically, phase 5 messages), a 406 error code will be returned if a message cannot be returned as Json.

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").

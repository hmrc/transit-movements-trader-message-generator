
# transit-movements-trader-message-generator

Create example NCTS messages.  Contents will be valid according to the XSD files but are not guaranteed to be valid because some elements depend on other contents - see DDNA rules.

Start locally
```aidl
sbt run
```

Example use
```
curl -X POST http://localhost:9000/messages/CC015B -o CC015B.xml
```

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").

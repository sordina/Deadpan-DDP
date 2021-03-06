test_collection = new Meteor.Collection("test")

if (Meteor.isClient) {
  Template.hello.helpers({
    counter: function () {
      return test_collection.findOne({name: "number"}).count
    }
  });
}

if (Meteor.isServer) {
  Meteor.startup(function () {

    function increment() {
      test_collection.update({name: "number"}, {$inc: {count: 1}})
    }

    if(! test_collection.findOne({name: "number"})) {
      test_collection.insert({name: "number", count: 0})
    }

    setInterval(Meteor.bindEnvironment(increment), 1000)
  });

  Meteor.methods({
    realMethod: function(x) {
      console.log("Client called realMethod")
      return "Hello World"
    },
    realMethod2: function(x) {
      console.log("Client called realMethod2")
      return "DEADBEEF"
    }
  })
}

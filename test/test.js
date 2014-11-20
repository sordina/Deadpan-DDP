test_collection = new Meteor.Collection("test")

if (Meteor.isClient) {
  Template.hello.helpers({
    counter: function () {
      return test_collection.find().count()
    }
  });
}

if (Meteor.isServer) {
  Meteor.startup(function () {

    counter = 0

    function add_item() {
      counter += 1
      name = "I am " + counter
      test_collection.insert({name: name, i: counter})
    }

    setInterval(Meteor.bindEnvironment(add_item), 1000)
  });
}

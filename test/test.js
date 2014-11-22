test_collection = new Meteor.Collection("test")

if (Meteor.isClient) {
  Template.hello.helpers({
    counter: function () {
      console.log( test_collection.findOne({name: "number"}))
      return test_collection.findOne({name: "number"}).count
    }
  });
}

if (Meteor.isServer) {
  Meteor.startup(function () {

    function add_item() {
      test_collection.update({name: "number"}, {$inc: {count: 1}})
    }

    if(! test_collection.findOne({name: "number"})) {
      test_collection.insert({name: "number", count: 0})
    }

    setInterval(Meteor.bindEnvironment(add_item), 1000)
  });
}

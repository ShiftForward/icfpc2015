var icfpServices = angular.module('icfpServices', ['ngResource']);

icfpServices.factory('Solution', ['$resource',
  function($resource){
    return $resource('http://icfp.int.shiftforward.eu', {}, {
      all: {
        method: 'GET',
        headers: {'Authorization': 'Basic OjgrRnNlZURjL1haTWtoaUlNdkEyeTV4Tk1TQ29jK0RBNFhXZVRlLzJYejA9'},
        isArray: true
      }
    });
  }]);
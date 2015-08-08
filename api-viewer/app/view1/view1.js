'use strict';

angular.module('myApp.view1', ['ngRoute'])

.config(['$routeProvider', function($routeProvider) {
  $routeProvider.when('/view1', {
    templateUrl: 'view1/view1.html',
    controller: 'View1Ctrl'
  });
}])

.controller('View1Ctrl', ['$scope', 'Solution', function($scope, Solution) {

    $scope.solutions = [];
    Solution.all().$promise.then(function(solutions) {
      var x = _
          .chain(solutions)
          .groupBy('problemId')
          .map(function(results) {
            var solution = {};

            solution.results = results;
            var data = _
                .chain(results)
                .map(function(result) {
                  return { y: (result.score || 0), x: new Date(result.createdAt).getTime() / 1000, problemId: result.problemId };
                })
                .sortBy(function(r) {return r.x;})
                .value();

            solution.options = {
              renderer: 'line',
              interpolation: 'linear',
              min: 'auto'
            };
            solution.features = {
              yAxis: {
                tickFormat: 'formatKMBT'
              },
              xAxis: {
                timeUnit: 'day'
              },
              hover: {
                formatter: function(series, x, y, z, d, e) {
                  var problemId = e.value.problemId;
                  var date = '<span class="date">' + new Date(x * 1000).toUTCString() + '</span>';
                  var swatch = '<span class="detail_swatch" style="background-color: ' + series.color + '"></span>';
                  return swatch + 'Problem Id ' + problemId + ': ' + y + ' points <br>' + date + '<br>';
                }
              }
            };

            solution.series = [{
              name: "score",
              color: 'steelblue',
              data: data
            }];

            //console.log(solution)

            return solution;
          })
          .value();

      console.log(x);
      $scope.solutions = x;
      setInterval(function() { $scope.$broadcast('rickshaw::resize') }, 100);
      setInterval(function() { $scope.$broadcast('rickshaw::resize') }, 500);
      setInterval(function() { $scope.$broadcast('rickshaw::resize') }, 999);
    });
}]);
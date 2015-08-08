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
            var scoreData = _
                .chain(results)
                .filter(function(r) { return r.score != null && r.powerScore != null; })
                .map(function(result) {
                  return { y: result.score, x: new Date(result.createdAt).getTime() / 1000, problemId: result.problemId };
                })
                .sortBy(function(r) { return r.x; })
                .value();

            var powerScoreData = _
                .chain(results)
                .filter(function(r) { return r.score != null && r.powerScore != null; })
                .map(function(result) {
                  return { y: result.powerScore, x: new Date(result.createdAt).getTime() / 1000, problemId: result.problemId };
                })
                .sortBy(function(r) { return r.x; })
                .value();

            var totalScoreData = _
                .chain(results)
                .filter(function(r) { return r.score != null && r.powerScore != null; })
                .map(function(result) {
                  return { y: (result.score + result.powerScore), x: new Date(result.createdAt).getTime() / 1000, problemId: result.problemId };
                })
                .sortBy(function(r) { return r.x; })
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
                  return swatch + series.name + ': ' + y + ' points <br>' + moment(new Date(x * 1000).toUTCString()).calendar() + '<br>';
                }
              }
            };

            solution.series = [{
              name: "Score",
              color: 'orange',
              data: scoreData
            },{
              name: "Power Score",
              color: 'green',
              data: powerScoreData
            },{
              name: "Total Score",
              color: 'steelblue',
              data: totalScoreData
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
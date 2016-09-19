define(
    [
        "angular",
        "./dataset-controllers",
        "./dataset-services"
    ],
    function (angular, controllers) {
        "use strict";

        var datasetRoutes = angular.module("dataset.routes", ["narthex.common", "dashboard.services"]);
        datasetRoutes.config(
            [
                "$routeProvider", "userResolve",
                function ($routeProvider, userResolve) {
                    $routeProvider.when(
                        "/dataset/:spec", {
                            templateUrl: "/narthex/assets/templates/dataset.html",
                            controller: controllers.DatasetCtrl,
                            resolve: userResolve,
                            reloadOnSearch: false
                        }
                    );
                }
            ]
        );

        var narthexDataset = angular.module("narthex.dataset", [
            "ngRoute",
            "dataset.routes",
            "dataset.services",
            "narthex.common"
        ]);

        narthexDataset.controller('TreeCtrl', controllers.TreeCtrl);
        narthexDataset.controller('NodeCtrl', controllers.NodeCtrl);

        var config = function config($rootScopeProvider) {
            $rootScopeProvider.digestTtl(15);
        };
        config.$inject = ['$rootScopeProvider'];
        narthexDataset.config(config);

        return narthexDataset;
    });



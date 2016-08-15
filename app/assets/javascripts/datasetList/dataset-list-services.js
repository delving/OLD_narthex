//===========================================================================
//    Copyright 2014 Delving B.V.
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//===========================================================================

define(["angular", "common"], function (angular) {
    "use strict";

    var mod = angular.module("datasetList.services", ["narthex.common"]);

    mod.service("datasetListService", [
        "$http", "$q", "playRoutes", "$location",
        function ($http, $q, playRoutes, $location) {
            var app = playRoutes.web.AppController;

            var rejection = function (reply) {
                if (reply.status == 401) { // unauthorized
                    $location.path('/');
                }
                else {
                    console.log('why', reply);
                    if (reply.data.problem) {
                        alert("Processing problem " + reply.status + " (" + reply.data.problem + ")");
                    }
                    else {
                        alert("Network problem " + reply.statusText);
                    }
                }
            };

            return {
                datasetSocket: function() {
                    return new WebSocket(app.datasetSocket().webSocketUrl());
                },
                create: function (spec, character, prefix) {
                    return app.createDataset(spec, character, prefix).get().then(
                        function (response) {
                            return response.data;
                        },
                        rejection
                    );
                },
                command: function (spec, command) {
                    return app.command(spec, command).get().then(
                        function (response) {
                            return response.data;
                        },
                        rejection
                    );
                },
                listDatasets: function () {
                    return app.listDatasets().get().then(
                        function (response) {
                            return response.data;
                        },
                        rejection
                    );
                },
                listPrefixes: function () {
                    return app.listPrefixes().get().then(
                        function (response) {
                            return response.data;
                        },
                        rejection
                    );
                },
                setDatasetProperties: function (spec, payload) {
                    return app.setDatasetProperties(spec).post(payload).then(
                        function (response) {
                            return response.data;
                        },
                        rejection
                    );
                },
                listSipFiles: function (spec) {
                    return app.listSipFiles(spec).get().then(
                        function (response) {
                            return response.data;
                        },
                        rejection
                    );
                },
                deleteLatestSipFile: function (spec) {
                    return app.deleteLatestSipFile(spec).get().then(
                        function (response) {
                            return response.data;
                        },
                        rejection
                    );
                }
            };
        }
    ]);

    /**
     * If the current route does not resolve, go back to the start page.
     */
    var handleRouteError = function ($rootScope, $location) {
        $rootScope.$on("$routeChangeError", function (e, next, current) {
            $location.path("/");
        });
    };
    handleRouteError.$inject = ["$rootScope", "$location"];
    mod.run(handleRouteError);
    return mod;
});

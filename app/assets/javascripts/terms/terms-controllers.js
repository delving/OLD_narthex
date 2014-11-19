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

define(["angular"], function (angular) {
    "use strict";

    var TermsCtrl = function ($rootScope, $scope, $location, $routeParams, termsService, $timeout, pageScroll) {

        function getSearchParams() {
            $scope.datasetName = $routeParams.datasetName;
            $scope.path = $routeParams.path;
            $scope.histogramSize = parseInt($routeParams.size || "100");
            $scope.activeView = $routeParams.view || "vocabulary";
            $scope.vocabulary = $routeParams.vocabulary;
        }

        function updateSearchParams() {
            $location.search({
                path: $scope.path,
                histogramSize: $scope.histogramSize,
                view: $scope.activeView,
                vocabulary: $scope.vocabulary
            });
        }

        getSearchParams();

        // local
        $scope.sourceEntry = undefined; // list selection
        $scope.sought = ""; // the field model
        $scope.mappings = {};
        $scope.show = "all";
        $scope.concepts = [];
        $scope.histogram = [];
        $scope.histogramVisible = [];

        $scope.scrollTo = function (options) {
            pageScroll.scrollTo(options);
        };

        termsService.datasetInfo($scope.datasetName).then(function (datasetInfo) {
            $scope.datasetInfo = datasetInfo;
            var recordContainer;
            if (datasetInfo.origin.type == 'origin-drop'){
                var recordRoot = datasetInfo.delimit.recordRoot;
                recordContainer = recordRoot.substring(0, recordRoot.lastIndexOf("/"));
            }
            else {
                recordContainer = "/pockets/pocket";
            }
            $scope.sourceUriPath = $scope.path.substring(recordContainer.length);
        });

        function filterHistogram() {
            var mapped = 0;
            var unmapped = 0;

            function hasMapping(entry) {
                var mapping = $scope.mappings[entry.sourceUri];
                var number = parseInt(entry.count);
                if (mapping) {
                    mapped += number;
                }
                else {
                    unmapped += number;
                }
                return mapping;
            }

            switch ($scope.show) {
                case "mapped":
                    $scope.histogramVisible = _.filter($scope.histogram, function (entry) {
                        return hasMapping(entry);
                    });
                    break;
                case "unmapped":
                    $scope.histogramVisible = _.filter($scope.histogram, function (entry) {
                        return !hasMapping(entry);
                    });
                    break;
                default:
                    $scope.histogramVisible = _.filter($scope.histogram, function (entry) {
                        hasMapping(entry);
                        return true;
                    });
                    break;
            }
            $scope.mapped = mapped;
            $scope.unmapped = unmapped;
            $scope.all = mapped + unmapped;
        }

        // preparations
        termsService.listSkos().then(function (data) {
            $scope.vocabularyList = data.list;
            if ($scope.vocabularyList.length == 1) {
                $scope.selectVocabulary($scope.vocabularyList[0])
            }
        });
        termsService.getMappings($scope.datasetName).then(function (data) {
            _.forEach(data.mappings, function (mapping) {
                $scope.mappings[mapping.source] = {
                    target: mapping.target,
                    vocabulary: mapping.vocabulary,
                    prefLabel: mapping.prefLabel
                }
            });
            termsService.histogram($scope.datasetName, $scope.path, $scope.histogramSize).then(function (data) {
                $scope.histogram = _.map(data.histogram, function (count) {
                    var sourceUri = $rootScope.orgId + "/" + $scope.datasetName + $scope.sourceUriPath + "/" + encodeURIComponent(count[1]);
                    return {
                        value: count[1],
                        count: count[0],
                        sourceUri: sourceUri
                    }
                });
            });
        });

        function searchSkos(value) {
            if (!value || !$scope.vocabulary) return;
            $scope.scrollTo({element: '#skos-term-list', direction: 'up'});
            termsService.searchSkos($scope.vocabulary, value).then(function (data) {
                $scope.conceptSearch = data.search;
                var mapping = $scope.mappings[$scope.sourceUri];
                if (mapping) {
                    $scope.concepts = _.flatten(_.partition(data.search.results, function (concept) {
                        return concept.uri === mapping.target;
                    }));
                }
                else {
                    $scope.concepts = data.search.results;
                }
            });
        }

        function searchRecords(value) {
            var body = {
                "path": $scope.path,
                "value": value
            };
            termsService.queryRecords($scope.datasetName, body).then(function (data) {
                $scope.records = data;
            });
        }

        $scope.vocabularyTab = function () {
            $scope.activeView = "vocabulary";
            updateSearchParams();
        };

        $scope.skosTab = function () {
            if ($scope.vocabulary) {
                $scope.activeView = "skos";
                if ($scope.sourceEntry) {
                    $scope.sought = $scope.sourceEntry.value;
                }
                updateSearchParams();
            }
            else {
                $scope.vocabularyTab();
            }
        };

        $scope.recordTab = function () {
            $scope.activeView = "record";
            if ($scope.sourceEntry) {
                searchRecords($scope.sourceEntry.value);
            }
            updateSearchParams();
        };

        $scope.selectSource = function (entry) {
            $scope.sourceEntry = entry;
            var mapping = $scope.mappings[entry.sourceUri];
            if (mapping && mapping.vocabulary != $scope.vocabulary) {
                $scope.selectVocabulary(mapping.vocabulary);
            }
            switch ($scope.activeView) {
                case "skos":
                    $scope.sought = entry.value; // will trigger searchSkos
                    break;
                case "record":
                    searchRecords(entry.value);
                    break;
            }
        };

        $scope.selectVocabulary = function (name) {
            $scope.vocabulary = name;
            $scope.skosTab();
        };

        $scope.selectSought = function (value) {
            $scope.sought = value;
        };

        $scope.$watch("sought", function (sought) {
            if (sought) searchSkos(sought);
        });

        $scope.$watch("show", function () {
            filterHistogram();
        });

        $scope.$watch("histogram", function () {
            filterHistogram();
        });

        $scope.$watch("activeView", updateSearchParams());

        $scope.setMapping = function (concept) {
            if (!($scope.sourceEntry && $scope.vocabulary)) return;
            var body = {
                source: $scope.sourceEntry.sourceUri,
                target: concept.uri,
                vocabulary: $scope.vocabulary,
                prefLabel: concept.prefLabel
            };
            if ($scope.mappings[$scope.sourceEntry.sourceUri]) { // it already exists
                body.remove = "yes";
            }
            termsService.setMapping($scope.datasetName, body).then(function (data) {
                console.log("set mapping returns", data);
                if (body.remove) {
                    delete $scope.mappings[$scope.sourceEntry.sourceUri]
                }
                else {
                    $scope.mappings[$scope.sourceEntry.sourceUri] = {
                        target: concept.uri,
                        vocabulary: $scope.vocabulary,
                        prefLabel: concept.prefLabel
                    };
                }
                filterHistogram();
            });
        };
    };

    TermsCtrl.$inject = ["$rootScope", "$scope", "$location", "$routeParams", "termsService", "$timeout", "pageScroll"];

    return {
        TermsCtrl: TermsCtrl
    };
});

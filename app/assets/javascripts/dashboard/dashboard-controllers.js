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

define(["angular"], function () {
    "use strict";

    /**
     * user is not a service, but stems from userResolve (Check ../user/dashboard-services.js) object used by dashboard.routes.
     */
    var DashboardCtrl = function ($scope, user, dashboardService, fileUpload, $location, $upload, $timeout) {

        $scope.user = user;
        $scope.uploading = false;
        $scope.files = [];
        $scope.checkDelay = 1000;
        $scope.lastStatusCheck = 0;
        $scope.percent = null;

        function timeSinceStatusCheck() {
            var now = new Date().getTime();
            return now - $scope.lastStatusCheck;
        }

        $scope.onFileSelect = function ($files) {
            //$files: an array of files selected, each file has name, size, and type.  Take the first only.
            if ($files.length) {
                var file = $files[0];
                $scope.uploading = true;
                $scope.upload = $upload.upload(
                    {
                        url: '/dashboard/upload', //upload.php script, node.js route, or servlet url
                        // method: POST or PUT,
                        // headers: {'header-key': 'header-value'},
                        // withCredentials: true,
                        data: {myObj: $scope.myModelObj},
                        file: file
                    }
                ).progress(
                    function (evt) {
                        if ($scope.uploading) $scope.percent = parseInt(100.0 * evt.loaded / evt.total);
                    }
                ).success(
                    function (data, status, headers, config) {
                        $scope.uploading = false;
                        $scope.percent = null;
                        fetchFileList();
                    }
                ).error(
                    function (data) {
                        $scope.uploading = false;
                        $scope.percent = null;
                        alert(data.problem);
                    }
                );
            }
        };

        function checkFileStatus(file) {
            dashboardService.status(file.name).then(function (data) {
                file.status = data;
                if (file && !file.status.complete) {
                    var interval = timeSinceStatusCheck();
                    if (interval > 1000) { // don't change the scope thing too often
                        $scope.lastStatusCheck = new Date().getTime();
                    }
                    console.log("continuing " + file.name);
                    file.checker = $timeout(
                        function () {
                            checkFileStatus(file)
                        },
                        $scope.checkDelay
                    );
                }
            }, function (problem) {
                if (problem.status == 404) {
                    alert("Processing problem with " + file.name);
                    fetchFileList()
                }
                else {
                    alert("Network problem " + problem.status);
                }
            })
        }

        function fetchFileList() {
            dashboardService.list().then(function (data) {
                _.forEach($scope.files, function (file) {
                    if (file.checker) {
                        $timeout.cancel(file.checker);
                        file.checker = undefined;
                        console.log("cancelling " + file.name);
                    }
                });
                $scope.files = data;
                _.forEach($scope.files, checkFileStatus);
            });
        }

        fetchFileList();

        $scope.viewFile = function (file) {
            $location.path("/dashboard/" + file.name);
        };

        $scope.deleteFile = function (file) {
            dashboardService.zap(file.name).then(function (data) {
                fetchFileList();
            });
        }

    };

    DashboardCtrl.$inject = [
        "$scope", "user", "dashboardService", "fileUpload", "$location", "$upload", "$timeout"
    ];

    String.prototype.hashCode = function () {
        var self = this;
        var hash = 0;
        if (self.length == 0) return hash;
        for (var i = 0; i < self.length; i++) {
            var c = self.charCodeAt(i);
            hash = ((hash << 5) - hash) + c;
            hash = hash & hash; // Convert to 32bit integer
        }
        return hash;
    };

    var FileDetailCtrl = function ($scope, $routeParams, $timeout, $location, dashboardService, userService) {

        if (!userService.getUser()) $location.path("/");

        $scope.fileName = $routeParams.fileName;
        $scope.columns = { left: 4, right: 8 };
        $scope.aligning = false;

        var absUrl = $location.absUrl();
        var email = userService.getUser().email.replace("@", "_");
        var serverUrl = absUrl.substring(0, absUrl.indexOf("#"));
        var garbage = ('narthex|' + email + '|' + $scope.fileName).hashCode().toString(16).substring(1);
        $scope.apiPrefix = serverUrl + 'api/' + garbage + '/' + email + '/' + $scope.fileName;

        $scope.goToDashboard = function () {
            $location.path("/dashboard");
        };

        $scope.setAligning = function (on) {
            $scope.aligning = on;
            if (on) {
                $scope.columns.left = 6;
                $scope.columns.right = 6;
            }
            else {
                $scope.columns.left = 4;
                $scope.columns.right = 8;
            }
        };

        function selectFirstWithValues(node) {
            if (node.lengths.length) {
                return node;
            }
            else {
                for (var index=0; index < node.kids.length; index++) {
                    var nodeWithValues = selectFirstWithValues(node.kids[index]);
                    if (nodeWithValues) return nodeWithValues;
                }
                return undefined;
            }
        }

        dashboardService.index($scope.fileName).then(function (data) {
            $scope.tree = data;
            var first = selectFirstWithValues(data);
            if (first) $scope.selectNode(first);
        });

        function removeChosen(node) {
            if (!node) return;
            if (node.chosen) node.chosen = undefined;
            _.forEach(node.kids, removeChosen);
        }

        $scope.selectNode = function (node, $event) {
            if ($event) $event.stopPropagation();
            if (!node.lengths.length) return;
            removeChosen($scope.tree);
            node.chosen = true;
            $scope.node = node;
            $scope.apiPath = $scope.apiPrefix + node.path.replace(":", "_").replace("@", "_");
            $scope.sampleSize = 100;
            $scope.histogramSize = 100;
            $scope.fetchLengths();
            dashboardService.nodeStatus($scope.fileName, node.path).then(function (data) {
                $scope.status = data;
//                if ($scope.activeView == "histogram") {
//                    $scope.fetchHistogram();
//                }
//                else {
//                    $scope.fetchSample();
//                }
            });
        };

        $scope.fetchLengths = function () {
            $scope.activeView = "lengths";
            $scope.sample = undefined;
            $scope.histogram = undefined;
        };

        $scope.getX = function () {
            return function (d) {
                return d[0];
            }
        };

        $scope.getY = function () {
            return function (d) {
                return d[1];
            }
        };

        $scope.getColor = function () {
            var lengthName = ["0", "1", "2", "3", "4", "5", "6-10", "11-15", "16-20", "21-30", "31-50", "50-100", "100-*"]; // from XRay.scala lengthRanges
            var noOfColors = lengthName.length;
            var frequency = 4 / noOfColors;

            function toHex(c) {
                var hex = c.toString(16);
                return hex.length == 1 ? "0" + hex : hex;
            }

            function rgbToHex(r, g, b) {
                return "#" + toHex(r) + toHex(g) + toHex(b);
            }

            var colorLookup = {};
            for (var walk = 0; walk < noOfColors; ++walk) {
                var r = Math.floor(Math.sin(frequency * walk + 0) * (127) + 128);
                var g = Math.floor(Math.sin(frequency * walk + 1) * (127) + 128);
                var b = Math.floor(Math.sin(frequency * walk + 3) * (127) + 128);
                colorLookup[lengthName[walk]] = rgbToHex(r, g, b);
            }
            return function (d, i) {
                return colorLookup[d.data[0]];
            };
        };

        $scope.fetchSample = function () {
            $scope.activeView = "sample";
            dashboardService.sample($scope.fileName, $scope.node.path, $scope.sampleSize).then(function (data) {
                $scope.sample = data;
                $scope.histogram = undefined;
            });
        };

        $scope.fetchHistogram = function () {
            $scope.activeView = "histogram";
            dashboardService.histogram($scope.fileName, $scope.node.path, $scope.histogramSize).then(function (data) {
                $scope.histogram = data;
                $scope.sample = undefined;
            });
        };

        $scope.isMoreSample = function () {
            if (!($scope.status && $scope.status.samples)) return false;
            var which = _.indexOf($scope.status.samples, $scope.sampleSize, true);
            return which < $scope.status.samples.length - 1;
        };

        $scope.moreSample = function () {
            var which = _.indexOf($scope.status.samples, $scope.sampleSize, true);
            $scope.sampleSize = $scope.status.samples[which + 1];
            $scope.fetchSample();
        };

        $scope.isMoreHistogram = function () {
            if (!($scope.status && $scope.status.histograms)) return false;
            var which = _.indexOf($scope.status.histograms, $scope.histogramSize, true);
            return which < $scope.status.histograms.length - 1;
        };

        $scope.moreHistogram = function () {
            var which = _.indexOf($scope.status.histograms, $scope.histogramSize, true);
            $scope.histogramSize = $scope.status.histograms[which + 1];
            $scope.fetchHistogram();
        };

        $scope.$watch("filterTyped", function (filterTyped, oldFilterTyped) {
            if ($scope.filterTicking) {
                $timeout.cancel($scope.filterTicking);
                $scope.filterTicking = undefined;
            }
            $scope.filterTicking = $timeout(
                function () {
                    $scope.filterText = filterTyped;
                },
                500
            );
        });

        /**
         * Scrolls up and down to a named anchor hash, or top/bottom of an element
         * @param {Object} options: hash - named anchor, element - html element (usually a div) with id
         * eg. scrollTo({'hash': 'page-top'})
         * eg. scrollto({'element': '#document-list-container'})
         */
        $scope.scrollTo = function (options) {
            options = options || {};
            var hash = options.hash || undefined,
                element = options.element || undefined,
                direction = options.direction || 'up';
            // navigate to hash
            if (hash) {
                var old = $location.hash();
                $location.hash(hash);
                $anchorScroll();
                $location.hash(old);//reset to old location in order to maintain routing logic (no hash in the url)
            }
            // scroll the provided dom element if it exists
            if (element && $(options.element).length) {
                var scrollElement = $(options.element);
                // get the height from the actual content, not the container
                var scrollHeight = scrollElement[0].scrollHeight;
                var distance = '';
                if (!direction || direction == 'up') {
                    distance = -scrollHeight;
                }
                else {
                    distance = scrollHeight;
                }
                $timeout(function () {
                    scrollElement.stop().animate({
                        scrollLeft: '+=' + 0,
                        scrollTop: '+=' + distance
                    });
                }, 250);
            }
        };
    };

    FileDetailCtrl.$inject = ["$scope", "$routeParams", "$timeout", "$location", "dashboardService", "userService"];

    var TreeCtrl = function ($scope) {
        $scope.$watch('tree', function (tree, oldTree) {
            if (tree) {
                $scope.node = tree;
            }
        });
    };

    TreeCtrl.$inject = ["$scope"];

    var TreeNodeCtrl = function ($scope) {
//        $scope.setNode = function(node) {
//            console.log("node", node);
//            $scope.node = node;
//        };
    };

    TreeNodeCtrl.$inject = ["$scope"];

    return {
        DashboardCtrl: DashboardCtrl,
        TreeCtrl: TreeCtrl,
        TreeNodeCtrl: TreeNodeCtrl,
        FileDetailCtrl: FileDetailCtrl
    };

});

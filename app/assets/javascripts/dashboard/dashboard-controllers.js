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

String.prototype.endsWith = function(suffix) {
    return this.indexOf(suffix, this.length - suffix.length) !== -1;
};

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
            if ($files.length && !$scope.uploading) {
                var file = $files[0];
                if (!(file.name.endsWith(".xml.gz") || file.name.endsWith(".xml"))) {
                    alert("File must end with .xml or .xml.gz");
                    return;
                }
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
                    function () {
                        $scope.uploading = false;
                        $scope.percent = null;
                        fetchFileList();
                    }
                ).error(
                    function (data, status, headers, config) {
                        $scope.uploading = false;
                        $scope.percent = null;
                        console.log("Failure during upload: data", data);
                        console.log("Failure during upload: status", status);
                        console.log("Failure during upload: headers", headers);
                        console.log("Failure during upload: config", config);
                        alert(data.problem);
                    }
                );
            }
        };

        function checkFileStatus(file) {
            dashboardService.datasetInfo(file.name).then(function (datasetInfo) {
                file.status = datasetInfo.status;
                file.delimit = datasetInfo.delimit;
                if (file.status.percent > 0 || file.status.workers > 0) {
                    var interval = timeSinceStatusCheck();
                    if (interval > 1000) { // don't change the scope thing too often
                        $scope.lastStatusCheck = new Date().getTime();
                    }
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

        function checkSaveStatus(file) {
            if (!file.datasetInfo) return false;
            var delimit = file.datasetInfo.delimit;
            return !!delimit.recordRoot;
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
                _.forEach($scope.files, checkSaveStatus);
            });
        }

        fetchFileList();

        $scope.viewFile = function (file) {
            $location.path("/dataset/" + file.name);
        };

        $scope.deleteFile = function (file) {
            dashboardService.zap(file.name).then(function () {
                fetchFileList();
            });
        };

        $scope.saveRecords = function (file) {
            dashboardService.saveRecords(file.name).then(function () {
                $timeout(
                    function () {
                        checkFileStatus(file)
                    },
                    $scope.checkDelay
                )
            });
        };

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

    var FileDetailCtrl = function ($scope, $routeParams, $timeout, $location, dashboardService, user) {
        var MAX_FOR_VOCABULARY = 12500;
        $scope.fileName = $routeParams.fileName;
        var absUrl = $location.absUrl();
        var email = user.email.replace("@", "_");
        var serverUrl = absUrl.substring(0, absUrl.indexOf("#"));
        var garbage = ('narthex|' + email + '|' + $scope.fileName).hashCode().toString(16).substring(1);
        $scope.apiPrefix = serverUrl + 'api/' + garbage + '/' + email + '/' + $scope.fileName;
        $scope.selectedNode = null;
        $scope.uniqueIdNode = null;
        $scope.recordRootNode = null;

        dashboardService.datasetInfo($scope.fileName).then(function(thing) {
            $scope.datasetInfo = thing;
            dashboardService.index($scope.fileName).then(function (tree) {
                function sortKids(node) {
                    if (!node.kids.length) return;
                    node.kids = _.sortBy(node.kids, function (kid) {
                        return kid.tag.toLowerCase();
                    });
                    for (var index = 0; index < node.kids.length; index++) {
                        sortKids(node.kids[index]);
                    }
                }

                sortKids(tree);

                var recordRoot = thing.delimit.recordRoot;
                var uniqueId = thing.delimit.uniqueId;

                function setDelim(node) {
                    if (node.path == recordRoot) {
                        $scope.recordRootNode = node;
                    }
                    else if (node.path == uniqueId) {
                        $scope.uniqueIdNode = node;
                    }
                    for (var index = 0; index < node.kids.length; index++) {
                        setDelim(node.kids[index]);
                    }
                }

                if (recordRoot) setDelim(tree);

                function selectNode(path, node) {
                    if (!path.length) {
                        $scope.selectNode(node);
                    }
                    else {
                        var tag = path.shift();
                        for (var index = 0; index < node.kids.length; index++) {
                            if (tag == node.kids[index].tag) {
                                selectNode(path, node.kids[index]);
                                return;
                            }
                        }
                    }
                }

                $scope.tree = tree;
                if ($routeParams.path) selectNode($routeParams.path.substring(1).split('/'), { tag: '', kids: [$scope.tree]});
            });
        });

        $scope.goToDashboard = function () {
            $location.path("/dashboard");
            $location.search({});
        };

        $scope.goToTerms = function () {
            $location.path("/terms/" + $scope.fileName);
            $location.search({
                path: $routeParams.path,
                size: $scope.status.histograms[$scope.status.histograms.length-1]
            });
        };

        function setActiveView(activeView) {
            $scope.activeView = activeView;
            $location.search({
                path: $routeParams.path,
                view: activeView
            });
        }

        function setActivePath(activePath) {
            $scope.activePath = activePath;
            $location.search({
                path: activePath,
                view: $routeParams.view
            });
        }

        $scope.selectNode = function (node, $event) {
            if ($event) $event.stopPropagation();
            if (!node.lengths.length) return;
            $scope.selectedNode = node;
            setActivePath(node.path);
            dashboardService.nodeStatus($scope.fileName, node.path).then(function (data) {
                $scope.status = data;
                var filePath = node.path.replace(":", "_").replace("@", "_");
                $scope.apiPath = $scope.apiPrefix + filePath;
                $scope.sampleSize = 100;
                $scope.histogramSize = 100;
                switch ($routeParams.view) {
                    case 'sample':
                        $scope.fetchSample();
                        break;
                    case 'histogram':
                        $scope.fetchHistogram();
                        break;
                    default:
                        $scope.fetchLengths();
                }
            });
        };

        $scope.setUniqueIdNode = function (node) {
            function selectFirstEmptyWithCount(node, count) {
                if (!node.lengths.length && node.count == count) {
                    return node;
                }
                else for (var index = 0; index < node.kids.length; index++) {
                    var emptyWithCount = selectFirstEmptyWithCount(node.kids[index], count);
                    if (emptyWithCount) return emptyWithCount;
                }
                return undefined;
            }

            var recordRootNode = selectFirstEmptyWithCount($scope.tree, node.count);
            if (recordRootNode) {
                $scope.recordRootNode = recordRootNode;
                $scope.uniqueIdNode = node;
                var body = {
                    recordRoot: $scope.recordRootNode.path,
                    uniqueId: $scope.uniqueIdNode.path,
                    recordCount: $scope.uniqueIdNode.count
                };
                dashboardService.setRecordDelimiter($scope.fileName, body).then(function () {
                    console.log("record delimiter set");
                });
            }
        };

        $scope.fetchLengths = function () {
            $scope.sample = undefined;
            $scope.histogram = undefined;
            setActiveView("lengths");
        };

        $scope.fetchSample = function () {
            dashboardService.sample($scope.fileName, $routeParams.path, $scope.sampleSize).then(function (data) {
                $scope.sample = data;
                $scope.histogram = undefined;
            });
            setActiveView("sample");
        };

        $scope.fetchHistogram = function () {
            dashboardService.histogram($scope.fileName, $routeParams.path, $scope.histogramSize).then(function (data) {
                _.forEach(data.histogram, function (entry) {
                    var percent = (100 * entry[0]) / $scope.selectedNode.count;
                    entry.push(percent);
                });
                $scope.histogram = data;
                $scope.sample = undefined;
                $scope.histogramUnique = data.histogram[0] && data.histogram[0][0] == 1;
                $scope.histogramVocabulary = (!$scope.histogramUnique) && ($scope.status.uniqueCount < MAX_FOR_VOCABULARY);
            });
            setActiveView("histogram");
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
            var lengthName = ["0", "1", "2", "3", "4", "5", "6-10", "11-15", "16-20", "21-30", "31-50", "50-100", "100-*"];
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
            return function (d) {
                return colorLookup[d.data[0]];
            };
        };

        /**
         * Scrolls up and down to a named anchor hash, or top/bottom of an element
         * {Object} options: hash - named anchor, element - html element (usually a div) with id
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

    FileDetailCtrl.$inject = ["$scope", "$routeParams", "$timeout", "$location", "dashboardService", "user"];

    var TreeCtrl = function ($scope) {
        $scope.$watch('tree', function (tree) {
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

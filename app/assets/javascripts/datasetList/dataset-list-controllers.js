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


String.prototype.endsWith = function (suffix) {
    return this.indexOf(suffix, this.length - suffix.length) !== -1;
};

define(["angular"], function () {
    "use strict";


    var progressStates = {
        'state-harvesting': "Harvesting",
        'state-collecting': "Collecting",
        'state-adopting': "Adopting",
        'state-generating': "Generating",
        'state-splitting': "Splitting",
        'state-collating': "Collating",
        'state-categorizing': "Categorizing",
        'state-processing': "Processing",
        'state-saving': "Saving",
        'state-skosifying': "Skosifying",
        'state-error': "Error"
    };

    /**
     * user is not a service, but stems from userResolve (Check ../user/dataset-list-services.js) object used by dashboard.routes.
     */
    var DatasetListCtrl = function ($rootScope, $scope, user, datasetListService, $location, pageScroll) {
        if (user == null) $location.path("/");
        $scope.user = user;
        $scope.uploading = false;
        $scope.datasets = [];
        $scope.percent = null;
        $scope.dropSupported = false;
        $scope.newFileOpen = false;
        $scope.newDataset = {};
        $scope.specFilter = "";
        $scope.stateFilter = "";
        $scope.socketSubscribers = {};

        var socket = datasetListService.datasetSocket();

        socket.onopen = function () {
            socket.send(user.username + " arrived on datasets page");
        };

        socket.onmessage = function (messageReturned) {
            var message = JSON.parse(messageReturned.data);
            var callback = $scope.socketSubscribers[message.datasetSpec];
            if (callback) {
                callback(message);
            }
            else {
                console.warn("Message for unknown dataset: " + message.datasetSpec);
            }
        };

        $scope.$on('$destroy', function () {
            socket.send(user.username + " left datasets page");
            socket.close();
        });

        $scope.subscribe = function (spec, callback) {
            $scope.socketSubscribers[spec] = callback;
        };

        $scope.unsubscribe = function (spec) {
            $scope.socketSubscribers[spec] = undefined;
        };

        function checkNewEnabled() {
            if ($scope.newDataset.specTyped)
                $scope.newDataset.spec = $scope.newDataset.specTyped.trim().replace(/\W+/g, "-").replace(/[-_]+/g, "-").toLowerCase();
            else
                $scope.newDataset.spec = "";
            $scope.newDataset.enabled = $scope.newDataset.spec.length && $scope.newDataset.character;
        }

        $scope.$watch("newDataset.specTyped", checkNewEnabled);
        $scope.$watch("newDataset.character", checkNewEnabled);


        /********************************************************************/
        /* dataset filtering                                                */
        /********************************************************************/
        $scope.showToolbar = false;
        $scope.toggleToolbar = function () {
            $scope.showToolbar = !$scope.showToolbar;
        }


        function filterDatasetBySpec(ds) {
            var filter = $scope.specFilter.trim();
            ds.visible = !filter || ds.datasetSpec.toLowerCase().indexOf(filter.toLowerCase()) >= 0;
        }

        function filterDatasetByState(ds) {
            var filter = $scope.stateFilter;
            if (filter != 'stateEmpty') {
                ds.visible = !filter || ds.stateCurrent.name === filter;
            }
            else {
                ds.visible = !filter || ds.empty;
            }
        }

        $scope.datasetListOrder = function (orderBy) {
            switch (orderBy) {
                case "state":
                    $scope.datasets = _.sortBy($scope.datasets, function (ds) {return ds.stateCurrent.name});
                    $scope.currentSortOrder = orderBy;
                    break;
                case "lastmodified":
                    $scope.datasets = _.sortBy($scope.datasets, function (ds) {return ds.stateCurrent.date}).reverse();
                    $scope.currentSortOrder = orderBy;
                    break;
                default:
                    $scope.datasets = _.sortBy($scope.datasets, 'datasetSpec');
                    $scope.currentSortOrder = 'spec';
                    break;
            }
            //console.log($scope.datasets)
        };

        $scope.setStateFilter = function(state){
            $scope.stateFilter = state;
            //filterDatasetByState(ds)
        };

        $scope.datasetVisibleFilter = function (ds) {
            return ds.visible;
        };

        $scope.$watch("specFilter", function () {
            _.each($scope.datasets, filterDatasetBySpec);
        });

        $scope.$watch("stateFilter", function () {
            _.each($scope.datasets, filterDatasetByState);
        });

        datasetListService.listPrefixes().then(function (prefixes) {
            $scope.characters = _.map(prefixes, function (prefix) {
                // for each prefix we should be able to accept a pre-mapped file
                return {
                    title: "Mapped to '" + prefix.toUpperCase() + "' format",
                    code: "character-mapped",
                    prefix: prefix
                };
            });
            if ($scope.characters.length == 1) {
                $scope.newDataset.character = $scope.characters[0];
            }
        });

        $scope.nonEmpty = function (obj) {
            return !_.isEmpty(obj)
        };

        $scope.isEmpty = function (obj) {
            return _.isEmpty(obj)
        };

        $scope.cancelNewFile = function () {
            $scope.newFileOpen = false;
        };

        $scope.setDropSupported = function () {
            $scope.dropSupported = true;
        };

        $scope.datasetStates = [
            {name: 'stateEmpty', label: 'Empty', count: 0},
            {name: 'stateRaw', label: 'Raw', count: 0},
            {name: 'stateRawAnalyzed', label: 'Raw analyzed', count: 0},
            {name: 'stateSourced', label: 'Sourced', count: 0},
            {name: 'stateMappable', label: 'Mappable', count: 0},
            {name: 'stateProcessable', label: 'Processable', count: 0},
            {name: 'stateInError', label: 'In error', count: 0},
            {name: 'stateProcessed', label: 'Processed', count: 0},
            {name: 'stateAnalyzed', label: 'Analyzed', count: 0},
            {name: 'stateSaved', label: 'Saved', count: 0},
            {name: 'stateIncrementalSaved', label: 'Incremental Saved', count: 0}
        ];

        $scope.decorateDataset = function (dataset) {
            dataset.edit = angular.copy(dataset);
            dataset.apiMappings = user.narthexAPI + '/' + dataset.datasetSpec + '/mappings';
            dataset.states = [];
//            if (dataset.character) dataset.prefix = info.character.prefix;
//            split the states into date and time
            var stateVisible = false;
            _.forEach(
                $scope.datasetStates,
                function (state) {
                    var time = dataset[state.name];
                    if (time) {
                        stateVisible = true;
                        var dt = time.split('T');
                        dataset.states.push({"name": state.name, "date": Date.parse(time)});
                        dataset[state.name] = {
                            d: dt[0],
                            t: dt[1].split('+')[0],
                            dt: dt
                        };
                    }
                }
            );
            if (!stateVisible) {
                dataset.empty = true;
            };
            dataset.stateCurrent = _.max(dataset.states, function (state) {
                return state.date
            });

            if (_.isEmpty(dataset.states)) {
                dataset.stateCurrent = {"name": "stateEmpty", "date": Date.now()};
            }
            if (!_.isUndefined(dataset.datasetErrorMessage)) {
                dataset.stateCurrent = {"name": "stateInError", "date": Date.now()};
            }
            //console.log(dataset, dataset.stateCurrent, dataset.states, dataset.datasetErrorMessage)
            dataset.showCounters = _.some(dataset.states, function (state) {
               return state.name == 'stateProcessed' || state.name == 'stateIncrementalSaved';
            });
            dataset.showMapTerms = _.some(dataset.states, function (state) {
                return state.name == 'stateProcessed' || state.name == 'stateIncrementalSaved';
            });
            filterDatasetBySpec(dataset);
            return dataset;
        };

        $scope.fetchDatasetList = function () {
            //console.log('fetching list');
            $scope.specFilter = "";
            datasetListService.listDatasets().then(function (array) {
                _.forEach(array, $scope.decorateDataset);
                $scope.datasets = array;
                $scope.updateDatasetStateCounter();
            });
            console.log( $scope.datasets);
        };

        $scope.fetchDatasetList();
        // if the url contains a hash with the dataset name, then be so nice as to scroll right to it.
        if($location.hash()){
            pageScroll.scrollTo({hash: $location.hash()});
        }

        $scope.updateDatasetList = function (dataset) {
            $scope.datasets = _.map($scope.datasets, function (ds) {
                if (ds.datasetSpec == dataset.datasetSpec){
                    ds = dataset;
                }
                return ds;
            });
        };

        $scope.updateDatasetStateCounter = function () {
            var datasetStateCounter = _.countBy($scope.datasets, function (dataset) {
                return dataset.stateCurrent.name;
            });
            //var datasetErrorCounter = _.countBy($scope.datasets, function (dataset) {
            //   return _.isUndefined(dataset.errorMessage);
            //});
            //console.log(datasetErrorCounter);
            $scope.datasetStates = _.map(
                $scope.datasetStates,
                function (state) {
                    if (_.has(datasetStateCounter, state.name)) {
                        state.count = datasetStateCounter[state.name];
                    }
                    else {
                        state.count = 0;
                    }
                    return state;
                }
            );
        };

        $scope.createDataset = function () {
            datasetListService.create($scope.newDataset.spec, $scope.newDataset.character.code, $scope.newDataset.character.prefix).then(function () {
                $scope.cancelNewFile();
                $location.search("dataset", $scope.newDataset.spec);
                $scope.newDataset.specTyped = $scope.newDataset.spec = undefined;
                $scope.fetchDatasetList();
            });
        };

    };

    DatasetListCtrl.$inject = [
        "$rootScope", "$scope", "user", "datasetListService", "$location", "pageScroll"
    ];

    // these lists must match with DsInfo.scala

    var metadataFields = [
        "datasetName", "datasetDescription", "datasetAggregator", "datasetOwner", "datasetLanguage", "datasetRights"
    ];

    var harvestFields = [
        "harvestType", "harvestURL", "harvestDataset", "harvestPrefix", "harvestSearch"
    ];

    var harvestCronFields = [
        "harvestPreviousTime", "harvestDelay", "harvestDelayUnit", "harvestIncremental"
    ];

    var idFilterFields = [
        "idFilterType", "idFilterExpression"
    ];

    var publishFields = [
        "publishOAIPMH", "publishIndex", "publishLOD"
    ];

    var categoriesFields = [
        "categoriesInclude"
    ];

    var harvestingStatisticsFields = [
        "lastFullHarvestTime", "processedValid", "processedInvalid",
        "lastIncrementalHarvestTime", "processedIncrementalValid", "processedIncrementalInvalid"
    ];

    var DatasetEntryCtrl = function ($scope, datasetListService, $location, $timeout, $upload, $routeParams) {
        if (!$scope.dataset) {
            alert("no dataset!");
            return;
        }
        $scope.subscribe($scope.dataset.datasetSpec, function (message) {
            function addProgressMessage(p) {
                var pre = progressStates[p.state] + " " + p.count.toString();
                var post = '';
                switch (p.type) {
                    case "progress-percent":
                        post = " %";
                        break;
                    case "progress-workers":
                        p.count = 100;
                        post = " worker(s)";
                        break;
                    case "progress-pages":
                        p.count = p.count % 100;
                        post = " page(s)";
                        break;
                }
                p.message = pre + post;
                if (p.count < 10) p.count = 10; // minimum space to write the text above
                return p;
            }

            $scope.datasetBusy = false;

            $scope.$apply(function () {
                if (message.progressState) {
                    $scope.dataset.progress = addProgressMessage({
                        state: message.progressState,
                        type: message.progressType,
                        count: parseInt(message.count)
                    });
                    $scope.datasetBusy = true;
                    //console.log("PROGRESS: " + message.datasetSpec, $scope.dataset.progress);
                }
                else {
                    console.log($scope.dataset);
                    $scope.dataset = $scope.decorateDataset(message);
                    $scope.updateDatasetList(message);
                    $scope.updateDatasetStateCounter();
                    $scope.datasetListOrder($scope.currentSortOrder);
                    //console.log("IDLE: " + message.datasetSpec, message);
                    $scope.datasetBusy = false;
                }
            });
        });

        $scope.$on('$destroy', function () {
            $scope.unsubscribe($scope.dataset.spec);
        });
        $scope.leftTabOpen = "metadata";
        $scope.rightTabOpen = $scope.dataset.harvestURL ? "harvest" : "drop";
        $scope.expanded = $routeParams.dataset == $scope.dataset.datasetSpec;
        $scope.idFilter = {};
        var baseUrl = $scope.user ? $scope.user.naveDomain : "http://unknown-nave-domain";
        $scope.searchLink = baseUrl + "/search?q=delving_spec:" + "\"" + $scope.dataset.datasetSpec + "\"";

        $scope.apiLink = baseUrl + "/api/search/v1/?q=delving_spec:" + $scope.dataset.datasetSpec;
        // todo: note that edm is hardcoded here:
        $scope.oaiPmhLink = baseUrl + "/api/oai-pmh?verb=ListRecords&metadataPrefix=edm&set=" + $scope.dataset.datasetSpec;
        $scope.apiPathErrors = $scope.user.narthexAPI + "/" + $scope.dataset.datasetSpec + "/errors";
        $scope.apiPathSourced = $scope.user.narthexAPI + "/" + $scope.dataset.datasetSpec + "/sourced";
        $scope.apiPathProcessed = $scope.user.narthexAPI + "/" + $scope.dataset.datasetSpec + "/processed";
        $scope.apiPathHarvestLog = $scope.user.narthexAPI + "/" + $scope.dataset.datasetSpec + "/log";
        $scope.apiDownloadSipZip = "/narthex/sip-app/" + $scope.dataset.datasetSpec;
        $scope.apiWebResourcePath = "/data/webresource/" + $scope.dataset.orgId + "/" + $scope.dataset.datasetSpec + "/source/";
        $scope.sparqlPath = $scope.user.naveDomain + "/snorql/?query=SELECT+%3Fs+%3Fp+%3Fo+%3Fg+WHERE+%7B%0D%0A++graph+%3Fg+%7B%0D%0A++++%3Fs1+%3Chttp%3A%2F%2Fcreativecommons.org%2Fns%23attributionName%3E+%22" + $scope.dataset.datasetSpec + "%22%0D%0A++%7D%0D%0A+++GRAPH+%3Fg+%7B%0D%0A++++++%3Fs+%3Fp+%3Fo+.%0D%0A+++%7D%0D%0A%7D%0D%0ALIMIT+50&format=browse";
        if($scope.dataset.harvestURL){
            $scope.pmhPreviewBase = $scope.dataset.harvestURL.replace('?', '') + "?verb=ListRecords&metadataPrefix=" + $scope.dataset.harvestPrefix;
        }
        if ($scope.dataset.harvestDataset) {
            $scope.pmhPreviewBase = $scope.pmhPreviewBase + "&set=" + $scope.dataset.harvestDataset;
        }
        function setUnchanged() {
            function unchanged(fieldNameList) {
                var unchanged = true;
                _.forEach(fieldNameList, function (fieldName) {
                    if (!angular.equals($scope.dataset[fieldName], $scope.dataset.edit[fieldName])) {
//                    console.log("changed " + fieldName);
                        unchanged = false;
                    }
                });
                return unchanged;
            }

            $scope.unchangedMetadata = unchanged(metadataFields);
            $scope.unchangedPublish = unchanged(publishFields);
            $scope.unchangedHarvest = unchanged(harvestFields);
            $scope.unchangedHarvestCron = unchanged(harvestCronFields);
            $scope.unchangedIdFilter = unchanged(idFilterFields);
            $scope.unchangedCategories = unchanged(categoriesFields);
            $scope.incrementalPreview  = $scope.dataset['harvestPreviousTime'];
        }

        $scope.$watch("dataset.edit", setUnchanged, true); // deep comparison

        $scope.$watch("dataset", function (newDs) {
            //console.log("watched ds="+$scope.dataset.datasetSpec);
            setUnchanged()
        });

        $scope.$watch("expanded", function (expanded) {
            if (expanded) {
                $location.search("dataset", $scope.dataset.datasetSpec);
            }
        });

        $scope.receiveDropped = function ($files) {
            //$files: an array of files selected, each file has name, size, and type.  Take the first only.
            if (!($files.length && !$scope.dataset.uploading)) return;
            var onlyFile = $files[0];
            if (!(onlyFile.name.endsWith('.xml.gz') || onlyFile.name.endsWith('.xml') || onlyFile.name.endsWith('.csv') || onlyFile.name.endsWith('.sip.zip'))) {
                alert("Sorry, the file must end with '.xml.gz', '.xml', '.csv' or '.sip.zip'");
                return;
            }
            $scope.dataset.uploading = true;
            $upload.upload({
                url: '/narthex/app/dataset/' + $scope.dataset.datasetSpec + '/upload',
                file: onlyFile
            }).progress(
                function (evt) {
                    if ($scope.dataset.uploading) $scope.dataset.uploadPercent = parseInt(100.0 * evt.loaded / evt.total);
                }
            ).success(
                function () {
                    $scope.dataset.uploading = false;
                    $scope.dataset.uploadPercent = null;
                }
            ).error(
                function (data, status, headers, config) {
                    $scope.dataset.uploading = false;
                    $scope.dataset.uploadPercent = null;
                    console.log("Failure during upload: data", data);
                    console.log("Failure during upload: status", status);
                    console.log("Failure during upload: headers", headers);
                    console.log("Failure during upload: config", config);
                    alert(data.problem);
                }
            );
        };

        $scope.showMetadataSubmitSuccess = false;

        function setProperties(propertyList) {
            var payload = {propertyList: propertyList, values: {}};
            _.forEach(propertyList, function (propertyName) {
                payload.values[propertyName] = angular.copy($scope.dataset.edit[propertyName]);
                //console.log(propertyName, angular.copy($scope.dataset.edit[propertyName]));
            });
            datasetListService.setDatasetProperties($scope.dataset.datasetSpec, payload);
            $scope.showMetadataSubmitSuccess = true;
        }

        $scope.submitMetadataForm = function (metadataform) {
            if (metadataform.$valid) {
                setProperties(metadataFields);
            }
        }

        $scope.setMetadata = function () {
            setProperties(metadataFields);
        };

        $scope.setPublish = function () {
            setProperties(publishFields);
        };

        $scope.setCategories = function () {
            setProperties(categoriesFields)
        };

        $scope.setHarvest = function () {
            setProperties(harvestFields);
        };

        $scope.setHarvestCron = function () {
            setProperties(harvestCronFields);
        };

        $scope.setIdFilter = function () {
            setProperties(idFilterFields);
        };

        $scope.isLater = function (currState, nextState) {
            if (!nextState) return true;
            if (currState) return currState.dt > nextState.dt;
            return false;
        };

        $scope.isCurrent = function (currState) {
            return currState == $scope.dataset.stateCurrent.name;
        };

        $scope.showLastSourcedPage = function () {
            window.open($scope.apiPathSourced, "_blank")
        };

        $scope.showLastProcessedPage = function () {
            window.open($scope.apiPathProcessed, "_blank")
        };

        $scope.showInvalidRecordsPage = function () {
            window.open($scope.apiPathErrors, "_blank")
        };

        $scope.showHarvestingLog = function () {
            window.open($scope.apiPathHarvestLog, "_blank")
        };

        $scope.goToDataset = function () {
            $location.path("/dataset/" + $scope.dataset.datasetSpec);
        };

        $scope.goToSparql = function () {
            window.open($scope.sparqlPath, "_blank")
        };

        $scope.goToTerms = function () {
            $location.path("/terms/" + $scope.dataset.datasetSpec);
        };

        $scope.goToCategories = function () {
            $location.path("/categories/" + $scope.dataset.datasetSpec);
        };

        function command(command, areYouSure, after) {
            if (areYouSure && !confirm(areYouSure)) return;
            datasetListService.command($scope.dataset.datasetSpec, command).then(function (reply) {
                console.log(reply);
            }).then(function () {
                if (after) after();
            });
        }

        $scope.interruptProcessing = function () {
            command("interrupt", "Interrupt processing?");
        };

        $scope.clearError = function () {
            command("clear error", null);
        };

        $scope.deleteDataset = function () {
            command("delete", "Delete dataset?", function () {
                $timeout($scope.fetchDatasetList, 2000);
            });
        };

        $scope.start = function (commandMessage, question) {
            $scope.datasetBusy = true;
            command(commandMessage, question);
        };

        $scope.remove = function (commandMessage, question) {
            command(commandMessage, question);
        };

        function fetchSipFileList() {
            datasetListService.listSipFiles($scope.dataset.datasetSpec).then(function (data) {
                $scope.sipFiles = (data && data.list && data.list.length) ? data.list : undefined;
            });
        }

        // fetchSipFileList();

        $scope.deleteSipZip = function () {
            datasetListService.deleteLatestSipFile($scope.dataset.datasetSpec).then(function () {
                fetchSipFileList();
            });
        };
        function executeIdFilter() {
            var expression = $scope.dataset.edit.idFilterExpression || '';
            var delimiter = ":::";
            var divider = expression.indexOf(delimiter);
            if (divider < 0) {
                $scope.idFilter.input = "";
                $scope.idFilter.error = "No divider";
                $scope.idFilter.output = "";
            }
            else {
                $scope.idFilter.error = "";
                var regExp = new RegExp(expression.substring(0, divider), 'g');
                var replacement = expression.substring(divider + delimiter.length);
                if($scope.idFilter.input){
                    $scope.idFilter.output = $scope.idFilter.input.replace(regExp, replacement);
                }
            }
        }

        $scope.$watch("idFilter.input", executeIdFilter);
        $scope.$watch("dataset.edit.idFilterExpression", executeIdFilter);

    };

    DatasetEntryCtrl.$inject = ["$scope", "datasetListService", "$location", "$timeout", "$upload", "$routeParams"];

    return {
        DatasetListCtrl: DatasetListCtrl,
        DatasetEntryCtrl: DatasetEntryCtrl
    };

});

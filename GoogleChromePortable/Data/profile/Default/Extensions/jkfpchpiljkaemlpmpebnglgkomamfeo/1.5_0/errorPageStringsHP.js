        function isExternalUrlSafeForNavigation(urlStr) {
            var regEx = new RegExp("^(http(s?)|ftp|file)://", "i");
            return regEx.exec(urlStr);
        }

        function getHashUrl() {
            var location = window.location.href;
            var poundIndex = location.indexOf('#');

            if (poundIndex != -1 && poundIndex + 1 < location.length && isExternalUrlSafeForNavigation(location.substring(poundIndex + 1))) {
                return (location.substring(poundIndex + 1));
            }
            return "";
        }

        function clickRefresh() {
            var hashUrl = getHashUrl();
            if (hashUrl != "") {
                window.location.replace(hashUrl);
            }
        }

        function launchNetworkCheck() {
            chrome.tabs.getCurrent(function (tab) {
                chrome.runtime.sendMessage({ greeting: "HPNetworkCheck", tabid: tab.id }, function (response) {
                    console.log(response.result);
                });
               }
            );
        }


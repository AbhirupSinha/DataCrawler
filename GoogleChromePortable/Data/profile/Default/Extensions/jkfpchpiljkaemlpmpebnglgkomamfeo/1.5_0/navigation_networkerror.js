function NavigationCollector() {
  chrome.webNavigation.onErrorOccurred.addListener(
        this.onErrorOccurredListener_.bind(this));
}

NavigationCollector.TabsStore = {};
NavigationCollector.CurrentTabId = -1;
NavigationCollector.CurrentTabError = "";

NavigationCollector.prototype = {
    onErrorOccurredListener_: function (data) {
        console.log("onErrorOccurredListener_");

        if (data.frameId != 0)
            return;

        //net::ERR_PROXY_CONNECTION_FAILED
        //net::ERR_INTERNET_DISCONNECTED
        if (data.error == "net::ERR_INTERNET_DISCONNECTED" || data.error == "net::ERR_PROXY_CONNECTION_FAILED")
        {
            NavigationCollector.TabsStore[data.tabId] = data.url;

            var error_url = chrome.extension.getURL("hperror.htm");

            chrome.tabs.update(data.tabId, { url: error_url});
        }

    }
};



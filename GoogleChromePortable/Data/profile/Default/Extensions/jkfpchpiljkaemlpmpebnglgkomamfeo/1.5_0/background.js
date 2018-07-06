// Copyright (c) 2012 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

/**
 * @filedescription Initializes the extension's background page.
 */

var nav = new NavigationCollector();


// Reset the navigation state on startup. We only want to collect data within a
// session.
chrome.runtime.onStartup.addListener(function() {
  nav.resetDataStorage();
  console.log("chrome.runtime.onStartup.addListener");
});

chrome.browserAction.onClicked.addListener(function (tab) {
  chrome.runtime.sendNativeMessage('com.hp.network.check',
  { launchPoint: "63", url: "" },
  function (response) {
    console.log("Received " + response);
  });

});

chrome.runtime.onMessage.addListener(
  function (request, sender, sendResponse) {
      console.log(request);
      console.log(sender.tab ?
                  "from a content script:" + sender.tab.url :
                  "from the extension");
      if (request.greeting == "HPNetworkCheck")
      {
          console.log(request.tabid);
          var orgUrl = NavigationCollector.TabsStore[request.tabid];
          chrome.runtime.sendNativeMessage('com.hp.network.check',
          { launchPoint: "64", url: orgUrl},
          function (response) {
              if (chrome.runtime.lastError)
                  console.log(response);
          });

          sendResponse({ result: "finished" });
      }
  });

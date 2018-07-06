document.addEventListener("DOMContentLoaded", function () {
    var fileref = document.createElement("link");
    fileref.setAttribute("rel", "stylesheet");
    fileref.setAttribute("type", "text/css");

    if (window.navigator.language == "ar" || window.navigator.language == "he") {
        fileref.setAttribute("href", "style-rtl.css");
        document.body.dir = "rtl";
    }
    else {
        fileref.setAttribute("href", "style.css");
    }
    document.head.appendChild(fileref);

    document.getElementById("title").innerText = chrome.i18n.getMessage('title');
    document.getElementById("connection_error_header").innerText = chrome.i18n.getMessage('connection_error_header');
    document.getElementById("message_title").innerText = chrome.i18n.getMessage('message_title');
    document.getElementById("fix").innerText = chrome.i18n.getMessage('fix');
    document.getElementById("click_button").innerText = chrome.i18n.getMessage('click_button');
    document.getElementById("below").innerText = chrome.i18n.getMessage('below');
    document.getElementById("diagnose").innerText = chrome.i18n.getMessage('diagnose');
    document.getElementById("error_desc").innerText = chrome.i18n.getMessage('error_desc');
    document.getElementById("about").innerText = chrome.i18n.getMessage('about');
    document.getElementById("hpnc_desc_1").innerText = chrome.i18n.getMessage('hpnc_desc_1');
    document.getElementById("hpnc_desc_2").innerText = chrome.i18n.getMessage('hpnc_desc_2');
    document.getElementById("hpnc_desc_3").innerText = chrome.i18n.getMessage('hpnc_desc_3');
    document.getElementById("hpnc_desc_4").innerText = chrome.i18n.getMessage('hpnc_desc_4');

    document.getElementById('diagnose').addEventListener('click', launchNetworkCheck);
});
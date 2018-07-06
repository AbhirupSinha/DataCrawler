//Code to send instrumentation ping when extension installed.
var pingURL;
chrome.runtime.onInstalled.addListener(function () {
    //console.log("onInstalled");    
});
chrome.management.onEnabled.addListener(function (ExtensionInfo) {
    //console.log("onEnabled");    

    if (!localStorage["BingDefaultsSet"]) {
        localStorage["BingDefaultsSet"] = "done";
        var PCCode = "UWDF";
        var MachineID = "EEF42D79F57F4E57AE3C8A2D67332C93";
        var OS = "6.2.8400.1";
        var LoggerVersion = "1.7.29.0";
        var Market = "en-us";
        var PackageCode = "DefaultPack";
        var currentdate = new Date();
        var InstalledDate = currentdate.getFullYear() + "-" + (currentdate.getMonth() + 1) + "-" + currentdate.getDate();
        var InstalledTime = currentdate.getHours() + ":" + currentdate.getMinutes() + ":" + currentdate.getSeconds() + ":" + currentdate.getMilliseconds();
        var cookieFound = false;

        //pingURL = 'http://g.ceipmsn.com/8SE/44?MI=8218585407C24E79A0D6758CC4CB8157&LV=1.7.29.0&OS=6.0.6002.1&TE=29&TV=is';
        pingURL = 'http://g.ceipmsn.com/8SE/44?';

        chrome.cookies.getAll({}, function (cookies) {

            for (var i in cookies) {
                cookieFound = false;
                if (cookies[i].name == "PCCode") {
                    PCCode = cookies[i].value;
                    cookieFound = true;
                }
                else if (cookies[i].name == "MachineID") {
                    MachineID = cookies[i].value;
                    cookieFound = true;
                }
                else if (cookies[i].name == "OS") {
                    OS = cookies[i].value;
                    cookieFound = true;
                }
                else if (cookies[i].name == "InstalledDate") {
                    InstalledDate = cookies[i].value;
                    cookieFound = true;
                }
                else if (cookies[i].name == "InstalledTime") {
                    InstalledTime = cookies[i].value;
                    cookieFound = true;
                }
                else if (cookies[i].name == "Market") {
                    Market = cookies[i].value;
                    cookieFound = true;
                }
                else if (cookies[i].name == "LoggerVersion") {
                    LoggerVersion = cookies[i].value;
                    cookieFound = true;
                }
                else if (cookies[i].name == "PackageCode") {
                    PackageCode = cookies[i].value;
                    cookieFound = true;
                }

                //Remove cookies value
                if (cookieFound) {
                    var url = "http" + (cookies[i].secure ? "s" : "") + "://" + cookies[i].domain + cookies[i].path;
                    chrome.cookies.remove({ "url": url, "name": cookies[i].name });
                }
            }
			localStorage["NewTab_PC"] = PCCode;
            //console.log("PCCode :" + PCCode + " MachineID: " + MachineID + " OS: " + OS + " InstalledDate: " + InstalledDate + " InstalledTime: " + InstalledTime);
            SendPing(PCCode, MachineID, OS, InstalledDate, InstalledTime, LoggerVersion, Market, PackageCode);
        });
    }

});

function SendPing(PCCode,MachineID,OS,InstalledDate,InstalledTime, LoggerVersion, Market, PackageCode)
{
	var chromeVersion = 'GC' + /Chrome\/([0-9.]+)/.exec(navigator.userAgent)[1];
	var TVData = 'TV=is'+ PCCode +'|pk'+ PackageCode +'|rt1|tm'+ Market +'|hd' + chromeVersion +',1|sd' + chromeVersion +',0|se|hp' + chromeVersion +',1|cd'+ InstalledDate +'|ct'+ InstalledTime;
	pingURL = pingURL + 'MI=' + MachineID + '&LV=' + LoggerVersion + '&OS=' + OS + '&TE=29&' + TVData;
	pingURL = pingURL.replace(/\|/g, "%7c");  // For HTML Encoding
	pingURL = pingURL.replace(/\,/g, "%2c");  // For HTML Encoding
	var xhr = new XMLHttpRequest();
	xhr.open("GET", pingURL, true);
	xhr.send();
}	

																		

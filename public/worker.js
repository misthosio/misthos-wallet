function logMessage(msg) {
  console.log("[Background Worker] - " + msg);
};

function compareArrays(a1, a2) {
  return a1.length==a2.length && a1.every((v,i)=> v === a2[i]);
};

function fetchAll(links) {
  return Promise.all(links.map(l => {
    logMessage("Fetching link: " + l);
    return fetch(l);
  })).then((responses) =>
    Promise.all(responses.filter(r => r.status == 200).map(r => r.text()))
  ).then(texts => texts.filter(t => t != null));
};

intervalId = -1;
links = [];

self.onmessage = ({data}) => {
  logMessage("Message received");
  newLinks = data.sort();
  if (compareArrays(links, newLinks) == false) {
    links = newLinks;
    fetchAll(links).then(texts => {
      postMessage(texts);
    });
    if (intervalId != -1) {
      clearInterval(intervalId);
    }
    intervalId = setInterval(function(){
      fetchAll(links).then(texts => {
          postMessage(texts);
        });
    }, 300000);
  }
};


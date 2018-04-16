function logMessage(msg) {
  console.log("[Sync Worker] - " + msg);
};

function fetchSummary(link) {
  logMessage("Fetching summary: " + link + "/summary.json");
  return fetch(link + "/summary.json").then(response => {
    if (response.status == 200) {
      return response.text().then(s => JSON.parse(s));
    } else {
      return null;
    }
  });
};

function fetchHistory(link) {
  logMessage("Fetching log: " + link + "/log.json");
  return fetch(link + "/log.json").then(response => {
    if (response.status == 200) {
      return response.text();
    } else {
      return null;
    }
  });
};

intervalId = -1;
links = [];
tenSecondsInMilliseconds = 10000;
syncInterval = tenSecondsInMilliseconds;

self.onmessage = ({data}) => {
  logMessage("Message received");

  if (intervalId != -1) {
    clearInterval(intervalId);
  }

  links = data.links.sort();
  knownItems = data.summary.knownItems.sort();

  links.forEach(link =>
    fetchSummary(link).then(summary => {
      if (summary != null) {
        otherItems = summary.knownItems.filter(i =>
          knownItems.includes(i) == false);
        if (otherItems.length > 0) {
          fetchHistory(link).then(history => {
            if (history != null) {
              postMessage(history)
            }
          })
        }
      }
    })
  );
  intervalId = setInterval(function(){
    links.forEach(link =>
      fetchSummary(link).then(summary => {
        if (summary != null) {
          otherItems = summary.knownItems.filter(i =>
            knownItems.includes(i) == false);
          if (otherItems.length > 0) {
            fetchHistory(link).then(history => {
              if (history != null) {
                postMessage(history)
              }
            })
          }
        }
      })
    );
  }, syncInterval);
};

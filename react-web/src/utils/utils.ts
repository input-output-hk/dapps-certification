import { BigNum } from "@emurgo/cardano-serialization-lib-browser";

export const exportObjectToJsonFile = (objectData: any) => {
    let filename = "Testing Report.json";
    let contentType = "application/json;charset=utf-8;";
    const navigator = window.navigator as any;
    if (window.navigator && navigator.msSaveOrOpenBlob) {
      var blob = new Blob(
        [decodeURIComponent(encodeURI(JSON.stringify(objectData)))],
        { type: contentType }
      );
      navigator.msSaveOrOpenBlob(blob, filename);
    } else {
      var a = document.createElement("a");
      a.download = filename;
      a.href =
        "data:" +
        contentType +
        "," +
        encodeURIComponent(JSON.stringify(objectData));
      a.target = "_blank";
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
    }
  };

export const formatToTitleCase = (value: string) => {
  if (value.indexOf(' ') !== -1) {
    return value.toLowerCase().split(' ').map((word) => {
      return word.replace(word[0], word[0].toUpperCase());
    }).join(' ');
  } else {
    const result = value.replace(/([A-Z])/g, " $1");
    const finalResult = result.charAt(0).toUpperCase() + result.slice(1);
    return finalResult.trim();
  }
}

export const formatTimeToReadable = (duration: number) => {
    const milliseconds = Math.floor(duration % 1000),
      seconds = Math.floor((duration / 1000) % 60),
      minutes = Math.floor((duration / (1000 * 60)) % 60),
      hours = Math.floor((duration / (1000 * 60 * 60)) % 24);
  
    let timeStr = '';
    if (hours) {
      timeStr += hours + 'h '
    }
    if (minutes) {
      timeStr += minutes + 'm '
    }
    if (seconds) {
      timeStr += seconds + 's '
    }
    if (milliseconds) {
      timeStr += milliseconds + 'ms'
    }
    return timeStr
}

export const convertAdaToLovelace = (fee_ada: number) => {
  return BigNum.from_str((fee_ada * 1000000).toString())
}
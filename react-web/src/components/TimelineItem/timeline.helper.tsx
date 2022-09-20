export const OutlineSvg = () => (
  <svg
    height="16"
    viewBox="0 0 16 16"
    version="1.1"
    width="16"
    aria-hidden="true"
  >
    <path
      fillRule="evenodd"
      d="M8 1.5a6.5 6.5 0 100 13 6.5 6.5 0 000-13zM0 8a8 8 0 1116 0A8 8 0 010 8z"
    ></path>
  </svg>
);

export const RunningSvg = () => (
  <svg
    width="16"
    height="16"
    fill="none"
    className="anim-rotate"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      opacity=".5"
      d="M8 15A7 7 0 108 1a7 7 0 000 14v0z"
      stroke="#DBAB0A"
      strokeWidth="2"
    ></path>
    <path d="M15 8a7 7 0 01-7 7" stroke="#DBAB0A" strokeWidth="2"></path>
    <path d="M8 12a4 4 0 100-8 4 4 0 000 8z" fill="#DBAB0A"></path>
  </svg>
);

export const PassedSvg = () => (
  <svg
    viewBox="0 0 16 16"
    version="1.1"
    width="16"
    height="16"
    aria-hidden="true"
  >
    <path
      fill="#009168"
      fillRule="evenodd"
      d="M8 16A8 8 0 108 0a8 8 0 000 16zm3.78-9.72a.75.75 0 00-1.06-1.06L6.75 9.19 5.28 7.72a.75.75 0 00-1.06 1.06l2 2a.75.75 0 001.06 0l4.5-4.5z"
    ></path>
  </svg>
);

export const FailedSvg = () => (
  <svg
    viewBox="0 0 16 16"
    version="1.1"
    width="16"
    height="16"
    aria-hidden="true"
  >
    <path
      fill="#FF493D"
      fillRule="evenodd"
      d="M2.343 13.657A8 8 0 1113.657 2.343 8 8 0 012.343 13.657zM6.03 4.97a.75.75 0 00-1.06 1.06L6.94 8 4.97 9.97a.75.75 0 101.06 1.06L8 9.06l1.97 1.97a.75.75 0 101.06-1.06L9.06 8l1.97-1.97a.75.75 0 10-1.06-1.06L8 6.94 6.03 4.97z"
    ></path>
  </svg>
);

export const setManyStatus = (
  index: number,
  config: any[],
  currentConfig: any,
  status: any,
  updatedState: string
) =>
  index < config.map((val: { status: any }) => val.status).indexOf(status)
    ? {
        ...currentConfig,
        state:
          currentConfig.state === "running" || currentConfig.state === "outline" // update only running status to updated status to avoid failure being overwritten
            ? updatedState
            : currentConfig.state,
      }
    : currentConfig;

export const processFinishedJson = (result: { [x: string]: any }) => {
  for (let key in result) {
    if (
      key.endsWith("Result") &&
      result[key] &&
      result[key].tag === "Failure"
    ) {
      let template = "<span class='error-title'>" +
          "<i>" + result[key].reason + "</i>" +
          // "<i class='arrow up'></i>" +
        "</span>";
      result[key].failingTestCase.forEach((val: string) => {
        template += "<span>Test Case: " + val + "</span>";
      });
      template += "<span>" + result[key].output + "</span>";

      const failedCase = document.createElement("div");
      failedCase.innerHTML = template;
      failedCase.classList.add("failure");
      document.getElementById("logContainer")!.append(failedCase);
    }
    else if (
      key.endsWith("Result") &&
      result[key] &&
      result[key].tag === "Success"
    ) {
      // show success results
    }
  }
};

export const indexOfExecutingProcess = (config: any[], state: string): number =>
  config.map((val: { state: any }) => val.state).indexOf(state) - 1;
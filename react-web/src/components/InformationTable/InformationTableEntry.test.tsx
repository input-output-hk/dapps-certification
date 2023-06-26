import React from "react";
import { render } from "@testing-library/react";
import InformationTableEntry from "./InformationTableEntry";

describe("InformationTableEntry", () => {
  test("renders the time and log correctly", () => {
    const time = "12:00 PM";
    const log = "Sample log";

    const { getByText } = render(
      <InformationTableEntry time={time} log={log} />
    );

    const timeElement = getByText(time);
    const logElement = getByText(log);

    expect(timeElement).toBeInTheDocument();
    expect(logElement).toBeInTheDocument();
  });
});
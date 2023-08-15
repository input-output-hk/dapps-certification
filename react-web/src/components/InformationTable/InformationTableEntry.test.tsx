import { render, screen } from "@testing-library/react";
import InformationTableEntry from "./InformationTableEntry";

describe("InformationTableEntry", () => {
  test("renders the time and log correctly", () => {
    const time = "12:00 PM";
    const log = "Sample log";

    render(
      <InformationTableEntry time={time} log={log} />
    );

    const timeElement = screen.getByText(time);
    const logElement = screen.getByText(log);

    expect(timeElement).toBeInTheDocument();
    expect(logElement).toBeInTheDocument();
  });
});
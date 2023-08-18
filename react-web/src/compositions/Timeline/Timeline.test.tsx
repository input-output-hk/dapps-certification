import React from "react";
import { render, screen } from "@testing-library/react";
import Timeline from "./Timeline";

jest.mock("store/store", () => ({
  useAppSelector: jest.fn((selectorFn) =>
    selectorFn({
      runTime: {
        buildInfo: {
          runState: "running",
          runTime: 1000,
        },
      },
    })
  ),
}));

describe("Timeline", () => {
  test("renders timeline items based on the status config", () => {
    const statusConfig = [
      { status: "pending", runTimeTaken: null },
      { status: "running", runTimeTaken: null },
      { status: "completed", runTimeTaken: null },
    ];
    const unitTestSuccess = true;
    const hasFailedTasks = false;

    render(
      <Timeline
        statusConfig={statusConfig}
        unitTestSuccess={unitTestSuccess}
        hasFailedTasks={hasFailedTasks}
      />
    );

    const timelineItems = screen.getAllByRole("listitem");
    expect(timelineItems).toHaveLength(statusConfig.length);
    statusConfig.forEach((config, index) => {
      expect(timelineItems[index]).toHaveTextContent(config.status);
      expect(timelineItems[index]).toHaveTextContent(String(config.runTimeTaken));
    });
  });

  test("sets runTimeTaken property based on the buildInfo runState", () => {
    const statusConfig = [
      { status: "pending", runTimeTaken: null },
      { status: "running", runTimeTaken: null },
      { status: "completed", runTimeTaken: null },
    ];
    const unitTestSuccess = true;
    const hasFailedTasks = false;
    const buildInfo = {
      runState: "running",
      runTime: 1000,
    };

    render(
      <Timeline
        statusConfig={statusConfig}
        unitTestSuccess={unitTestSuccess}
        hasFailedTasks={hasFailedTasks}
        buildInfo={buildInfo}
      />
    );

    // Assert that the runTimeTaken property is set correctly for the running status
    const runningTimelineItem = screen.getByTestId("undefined-runTimeTaken");
    expect(runningTimelineItem.textContent).toBe("1000");
  });
});
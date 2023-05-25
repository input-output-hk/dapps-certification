import { render, screen } from "@testing-library/react";
import Timeline from "./Timeline";
import { TIMELINE_CONFIG } from "./timeline.config";

const mockData = {
  status: "queued",
  text: "Queued",
  state: "running",
};

describe("renders the Timeline component", () => {
  it("renders component successfully", () => {
    render(<Timeline statusConfig={TIMELINE_CONFIG}
        unitTestSuccess={true}
        hasFailedTasks={false} />);
    expect(screen.getByTestId("Preparing")).toBeInTheDocument();
  });
})
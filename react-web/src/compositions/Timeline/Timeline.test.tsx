import { render, screen } from "@testing-library/react";
import Timeline from "./Timeline";
import { TIMELINE_CONFIG } from "./timeline.config";

test("renders Timeline component", () => {
    render(<Timeline statusConfig={TIMELINE_CONFIG} />);
    expect(screen.getByTestId("statusTimeline")).toBeInTheDocument();
});
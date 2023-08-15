import { fireEvent, screen, render } from "@testing-library/react";
import InformationTable from "./InformationTable";

const logs = [
  {
    Time: "2023-06-26T09:27:09.50524067Z",
    Source: "plutus-certification/generate-flake",
    Text: "flake.nix written at /tmp/generate-flake-573f61aa7ad13301",
  },
  {
    Time: "2023-06-26T09:27:09.506350585Z",
    Source: "plutus-certification/generate-flake",
    Text: "outputs.nix written at /tmp/generate-flake-573f61aa7ad13301",
  },
  {
    Time: "2023-06-26T09:27:09.506997313Z",
    Source: "plutus-certification/generate-flake",
    Text: "Certify.nix written at /tmp/generate-flake-573f61aa7ad13301",
  },
  {
    Time: "2023-06-26T09:27:09.929228957Z",
    Source: "plutus-certification/build-flake",
    Text: JSON.stringify({ key: { fields: {} } }),
  },
];

describe("InformationTable", () => {
  beforeEach(() => {
    window.HTMLElement.prototype.scrollIntoView = jest.fn(); // scrollIntoView is not implemented in jsdom
  });

  test("renders the logs correctly", () => {
    render(<InformationTable logs={logs} />);

    logs.forEach((log) => {
      const timeElement = screen.getByText(log.Time, { exact: false });
      const logElement = screen.getByText(log.Text, { exact: false });

      expect(timeElement).toBeInTheDocument();
      expect(logElement).toBeInTheDocument();
    });
  });

  test("scrolls to the bottom when logs change", () => {
    const { rerender } = render(<InformationTable logs={logs} />);

    logs.push({
      Time: "2023-06-26T09:27:09.929228757Z",
      Source: "plutus-certification/build-flake",
      Text: "warning: creating lock file '/tmp/generate-flake-573f61aa7ad13301/flake.lock'\n",
    });

    rerender(<InformationTable logs={logs} />);

    expect(window.HTMLElement.prototype.scrollIntoView).toHaveBeenCalledTimes(
      1
    );
    expect(window.HTMLElement.prototype.scrollIntoView).toHaveBeenCalledWith({
      behavior: "smooth",
    });
  });

  test("shows log view when 'View logs' button is clicked", () => {
    render(
      <InformationTable logs={logs} />
    );

    const viewLogsButton = screen.getByText("View logs");
    fireEvent.click(viewLogsButton);

    expect(screen.queryByTestId("log-information")).not.toHaveClass("hidden");
  });

  test("hides log view when 'Minimize' button is clicked", () => {
    render(
      <InformationTable logs={logs} />
    );

    const viewLogsButton = screen.getByText("View logs");
    fireEvent.click(viewLogsButton);

    const minimizeButton = screen.getByText("-");
    fireEvent.click(minimizeButton);

    expect(screen.queryByTestId("log-information")).toHaveClass("hidden");
  });

  test("renders the logs correctly with its texts", () => {
    const logs = [
      {
        Time: "2023-06-26T09:27:09.929228757Z",
        Source: "plutus-certification/build-flake",
        Text: JSON.stringify({
          key: { fields: { "launch-config": "Launch configuration log" } },
        }),
      },
      {
        Time: "2023-06-26T09:27:09.929228857Z",
        Source: "plutus-certification/build-flake",
        Text: JSON.stringify({
          key: { fields: { "chunk-data": "Chunk data log" } },
        }),
      },
    ];

    render(<InformationTable logs={logs} />);

    expect(
      screen.getByText(/Launch configuration log/i, { exact: false })
    ).toBeInTheDocument();
    expect(screen.getByText(/Chunk data log/i, { exact: false })).toBeInTheDocument();
  });
});
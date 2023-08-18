import { act, cleanup, fireEvent, render, screen } from "@testing-library/react";
import ColViz from "./ColViz";

const columns = [
  {
    Header: "Repo URL",
    accessor: "repoUrl",
    columnVisible: true,
  },
  {
    Header: "Commit Hash",
    accessor: "commitHash",
    disableSortBy: true,
  },
  {
    Header: "Commit Date",
    accessor: "commitDate",
    columnVisible: false,
  },
  {
    Header: "",
    disableSortBy: true,
    accessor: "delete",
  },
];

describe("Table column component", () => {
  afterEach(() => {
    cleanup();
  });

  it("renders component", () => {
    const updateColumnOptions = jest.fn();
    render(
      <ColViz columns={columns} updateColumnOptions={updateColumnOptions} />
    );

    columns
      .filter((column) => column.Header.length)
      .forEach((column) => {
        expect(screen.getByText(column.Header)).toBeInTheDocument();
      });
  });

  it("checks checkbox if column is visible", () => {
    const updateColumnOptions = jest.fn();
    render(
      <ColViz columns={columns} updateColumnOptions={updateColumnOptions} />
    );

    columns
      .filter(
        (column) => column.Header.length && column?.columnVisible !== false
      )
      .forEach((column) => {
        const checkBox = screen.getByTestId(`${column.Header}-checkbox`).firstChild as HTMLInputElement;
        expect(checkBox).toBeChecked();
      });
  });

  it("updates column config when checkbox is selected", async () => {
    const updateColumnOptions = jest.fn();
    render(
      <ColViz columns={columns} updateColumnOptions={updateColumnOptions} />
    );

    const columnOption = screen.getByTestId(`${columns[2].Header}-checkbox`);
    fireEvent.click(columnOption);

    expect(updateColumnOptions).toHaveBeenCalled();
    expect(updateColumnOptions).toHaveBeenCalledWith([
      {
        Header: "Repo URL",
        accessor: "repoUrl",
        columnVisible: true,
        checkBoxDisabled: false,
      },
      {
        Header: "Commit Hash",
        accessor: "commitHash",
        disableSortBy: true,
        columnVisible: true,
        checkBoxDisabled: false,
      },
      {
        Header: "Commit Date",
        accessor: "commitDate",
        columnVisible: true,
        checkBoxDisabled: false,
      },
      {
        Header: "",
        disableSortBy: true,
        accessor: "delete",
        columnVisible: true,
        checkBoxDisabled: false,
      },
    ]);
  });

  it("deselect checkbox when checked item is clicked on", async () => {
    const mock = jest.fn();
    render(
      <ColViz
        columns={[
          {
            Header: "Repo URL",
            accessor: "repoUrl",
            columnVisible: true,
          },
          {
            Header: "Commit Hash",
            accessor: "commitHash",
            disableSortBy: true,
            columnVisible: true,
          },
          {
            Header: "Commit Date",
            accessor: "commitDate",
            columnVisible: false,
          },
          {
            Header: "",
            disableSortBy: true,
            accessor: "delete",
            columnVisible: true,
          },
        ]}
        updateColumnOptions={mock}
      />
    );

    const columnOption = screen.getByTestId(`${columns[1].Header}-checkbox`);
    fireEvent.click(columnOption);

    expect(columnOption.firstChild).not.toBeChecked();
    expect(mock).toHaveBeenCalledWith([
      {
        Header: "Repo URL",
        accessor: "repoUrl",
        columnVisible: true,
        checkBoxDisabled: true,
      },
      {
        Header: "Commit Hash",
        accessor: "commitHash",
        disableSortBy: true,
        columnVisible: false,
      },
      {
        Header: "Commit Date",
        accessor: "commitDate",
        columnVisible: false,
      },
      {
        Header: "",
        disableSortBy: true,
        accessor: "delete",
        columnVisible: true,
      },
    ]);
  });

  it("disables checkbox when only one item is checked", async () => {
    render(
      <ColViz
        columns={[
          {
            Header: "Repo URL",
            accessor: "repoUrl",
          },
          {
            Header: "Commit Hash",
            accessor: "commitHash",
            disableSortBy: true,
          },
          {
            Header: "Commit Date",
            accessor: "commitDate",
            columnVisible: false,
          },
          {
            Header: "",
            disableSortBy: true,
            accessor: "delete",
            columnVisible: false,
          },
        ]}
        updateColumnOptions={() => {}}
      />
    );

    const columnOption = screen.getByTestId(`${columns[1].Header}-checkbox`);
    fireEvent.click(columnOption);

    const checkbox = screen.getByTestId(`${columns[0].Header}-checkbox`).firstChild as HTMLInputElement;

    expect(checkbox).toBeDisabled();
  });
});
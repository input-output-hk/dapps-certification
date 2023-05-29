import {
    cleanup,
    fireEvent,
    render,
    screen
  } from "@testing-library/react";
  import TableComponent from "./Table";
  import { act } from "react-dom/test-utils";
  import userEvent from "@testing-library/user-event";
  
  const data = [
    {
      adaUsdPrice: 0.371,
      endDate: "2024-05-18T16:52:06.780295123Z",
      features: [
        {
          id: "l1-run",
          name: "allows running testing campaign for a DApp",
        },
        {
          id: "l2-upload-report",
          name: "allows an auditor to upload a report",
        },
      ],
      id: "15",
      name: "Standard",
      price: 5390836,
      profileId: "10",
      startDate: "2023-05-19T16:52:06.780295123Z",
      status: "active",
      tierId: "2",
      type: "auditor",
    },
  ];
  
  const columns = [
    {
      Header: "Type of Subscription",
      accessor: "name",
    },
    {
      Header: "Amount Paid",
      accessor: "adaUsdPrice",
    },
    {
      Header: "Status",
      accessor: "status",
    },
    {
      Header: "Date of Subscription",
      accessor: "startDate",
    },
    {
      Header: "Date of Expiry",
      accessor: "endDate",
    },
  ];
  
  describe("Table component", () => {
    afterEach(() => {
      cleanup();
    });
  
    it("renders correctly", () => {
      const updateMyData = jest.fn();
      const skipPageReset = jest.fn();
  
      render(
        <TableComponent
          dataSet={data}
          columns={columns}
          updateMyData={updateMyData}
          skipPageReset={skipPageReset}
        />
      );
  
      expect(screen.getByTestId("tableComp")).toBeInTheDocument();
      columns
        .filter((column) => column.Header.length)
        .forEach((column) => {
          expect(screen.getByText(column.Header)).toBeInTheDocument();
        });
    });
  
    it("show colViz", () => {
      const updateMyData = jest.fn();
      const skipPageReset = jest.fn();
  
      render(
        <TableComponent
          dataSet={data}
          columns={columns}
          showColViz
          updateMyData={updateMyData}
          skipPageReset={skipPageReset}
        />
      );
  
      expect(screen.getByAltText("colvis")).toBeInTheDocument();
    });
  
    it("hide colViz", () => {
      const updateMyData = jest.fn();
      const skipPageReset = jest.fn();
  
      render(
        <TableComponent
          dataSet={data}
          columns={columns}
          updateMyData={updateMyData}
          skipPageReset={skipPageReset}
        />
      );
  
      expect(screen.queryByAltText("colvis")).not.toBeInTheDocument();
    });
  
    it("toggle visibility of colviz on button click", async () => {
      const updateMyData = jest.fn();
      const skipPageReset = jest.fn();
  
      render(
        <TableComponent
          dataSet={data}
          columns={columns}
          showColViz
          updateMyData={updateMyData}
          skipPageReset={skipPageReset}
        />
      );
  
      const colVizContainer = screen.getByTestId("colviz-container");
      expect(colVizContainer.style.display).toBe("none");
  
      const toggleBtn = screen.getByTestId("colviz-sideBarButton");
      await act(() => {
        fireEvent.click(toggleBtn);
      });
      expect(colVizContainer.style.display).toBe("block");
    });
  
    it("displays the correct number of rows per page", async () => {
      render(
        <TableComponent dataSet={Array(20).fill(data[0])} columns={columns} />
      );
  
      // Click on row select dropdown
      await userEvent.click(screen.getByRole("button", { name: /5/ }));
  
      const options = screen.getAllByRole("option");
      expect(options.length).toBe(3); // 3 rowsPerPageOptions
  
      // Click on 10 rows per page option
      await userEvent.click(options[1]);
  
      const rows = screen.getAllByRole("row");
      expect(rows).toHaveLength(11); // Header row + 10 data rows
    });
  
    it("changes the page when clicking on pagination buttons", () => {
      render(
        <TableComponent dataSet={Array(8).fill(data[0])} columns={columns} />
      );
  
      const nextPageButton = screen.getByLabelText("Go to next page");
      const previousPageButton = screen.getByLabelText("Go to previous page");
  
      const rows = screen.getAllByRole("row");
      expect(rows).toHaveLength(6); // Header row + 5 data rows
  
      fireEvent.click(nextPageButton);
  
      const updatedRows = screen.getAllByRole("row");
      expect(updatedRows).toHaveLength(4); // Header row + 3 data rows
  
      fireEvent.click(previousPageButton);
  
      const restoredRows = screen.getAllByRole("row");
      expect(restoredRows).toHaveLength(6); // Header row + 5 data rows
    });
  });
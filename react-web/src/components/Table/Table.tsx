import React, { useMemo, FC, useState } from "react";
import Table from "@mui/material/Table";
import TableBody from "@mui/material/TableBody";
import { usePagination, useTable, useSortBy } from "react-table";
import TableCell from "@mui/material/TableCell";
import TableContainer from "@mui/material/TableContainer";
import TableHead from "@mui/material/TableHead";
import TableRow from "@mui/material/TableRow";
import Paper from "@mui/material/Paper";
import "./Table.scss";
import ColViz from "./components/ColViz/ColViz";


const TableComponent: FC<any> = ({ dataSet, config, showColViz }) => {
  const data = useMemo(() => dataSet, [dataSet]);
  const columns = useMemo(() => config, [config]);
  const [showPivot, setShowPivot] = useState(false);
  const tableInstance = useTable({ columns, data }, useSortBy);
  const {
    getTableProps,
    getTableBodyProps,
    headerGroups,
    rows,
    prepareRow,
    setHiddenColumns,
  } = tableInstance;

  const updateColumnOptions = (updatedColumnsList: any) => {
    const hideColumns = updatedColumnsList
      .filter((column: any) => column.columnVisible !== true)
      .map((item: any) => item.accessor);
    setHiddenColumns(hideColumns);
  };

  return (
    <TableContainer component={Paper} className="table-component">
      <Table
        sx={{ minWidth: 650 }}
        aria-label="simple table"
        {...getTableProps()}
      >
        <TableHead>
          {headerGroups.map((headerGroup: any) => (
            <TableRow {...headerGroup.getHeaderGroupProps()}>
              {headerGroup.headers.map((column: any) => (
                <TableCell
                  {...column.getHeaderProps(column.getSortByToggleProps())}
                >
                   <div className="col-header">
                    <div> {column.render("Header")}</div>

                    <div>
                      {column.isSorted ? (
                        column.isSortedDesc ? (
                          <img
                            className="sort-icon"
                            src="images/descIcon.svg"
                            alt="descIcon"
                          />
                        ) : (
                          <img
                            className="sort-icon"
                            src="images/ascIcon.svg"
                            alt="ascIcon"
                          />
                        )
                      ) : (
                        ""
                      )}
                    </div>
                  </div>
                </TableCell>
              ))}
            </TableRow>
          ))}
        </TableHead>
        <TableBody {...getTableBodyProps()}>
          {rows.map((row: any) => {
            prepareRow(row);
            return (
              <TableRow
                sx={{ "&:last-child td, &:last-child th": { border: 0 } }}
                {...row.getRowProps()}
              >
                {row.cells.map((cell: any) => {
                  return (
                    <TableCell
                      {...cell.getCellProps()}
                    >
                      {cell.render("Cell")}
                    </TableCell>
                  );
                })}
              </TableRow>
            );
          })}
        </TableBody>
        {showColViz && (
          <div className="sideBar">
            <div
              style={{
                display: showPivot ? "block" : "none",
              }}
              className="colviz-container"
            >
              <ColViz
                columns={columns}
                updateColumnOptions={updateColumnOptions}
              />
            </div>
            <div className="sidebarButtons">
              <button
                className="sideBarButton"
                onClick={(e) => setShowPivot(!showPivot)}
              >
                <span className="sideBarButtonIcon">
                  <img src="images/grid.svg" alt="Column Selector" title="Column Selector" />
                </span>
                {/* <span className="sideBarButtonLabel">Customize Columns</span> */}
              </button>
            </div>
          </div>
        )}
      </Table>
      {data.length === 0 && <div className="no-data">No data found</div>}
    </TableContainer>
  );
};

export default TableComponent;
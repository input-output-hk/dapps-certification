import React, { useMemo, FC, useState } from "react";
import Table from "@mui/material/Table";
import TableBody from "@mui/material/TableBody";
import { usePagination, useTable, useSortBy } from "react-table";
// import {
//   TableInstance,
//   UsePaginationInstanceProps,
//   UsePaginationState,
// } from "react-table";
import TableCell from "@mui/material/TableCell";
import TableContainer from "@mui/material/TableContainer";
import TableHead from "@mui/material/TableHead";
import TableRow from "@mui/material/TableRow";
import Box from "@mui/material/Box";
import TablePagination from "@mui/material/TablePagination";
import Paper from "@mui/material/Paper";
import "./Table.scss";
import ColViz from "./components/ColViz/ColViz";


// export type PaginationTableInstance<T extends object> = TableInstance<T> &
//   UsePaginationInstanceProps<T> & {
//     state: UsePaginationState<T>;
//   };

// const TableComponent: FC<any> = ({ dataSet, config, showColViz }) => {
const TableComponent: FC<any> = ({
  dataSet,
  columns,
  showColViz,
  updateMyData,
  skipPageReset,
}) => {
  const data = useMemo(() => dataSet, [dataSet]);
  const [showPivot, setShowPivot] = useState(false);
  const [pageNo, setPageNo] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(5);
  const {
    getTableProps,
    getTableBodyProps,
    headerGroups,
    prepareRow,
    setHiddenColumns,
    page,
    gotoPage,
    setPageSize,
    state: { pageIndex, pageSize },
  } = useTable(
    {
      columns,
      data,
      // use the skipPageReset option to disable page resetting temporarily
      autoResetPage: !skipPageReset,
      // updateMyData isn't part of the API, but
      // anything we put into these options will
      // automatically be available on the instance.
      // That way we can call this function from our
      // cell renderer!
      updateMyData,
      initialState: { pageIndex: 0, pageSize: 5 } as any,
    },
    useSortBy,
    usePagination
  );
  // ) as PaginationTableInstance<any>;

  const updateColumnOptions = (updatedColumnsList: any) => {
    const hideColumns = updatedColumnsList
      .filter((column: any) => column.columnVisible !== true)
      .map((item: any) => item.accessor);
    setHiddenColumns(hideColumns);
  };

  const handleChangePage = (event: unknown, newPage: number) => {
    setPageNo(newPage);
    gotoPage(newPage);
  };

  const handleChangeRowsPerPage = (
    event: React.ChangeEvent<HTMLInputElement>
  ) => {
    setRowsPerPage(parseInt(event.target.value, 10));
    setPageSize(Number(event.target.value));
    setPageNo(0);
  };


  return (
    <Box sx={{ width: "100%" }}>
      <Paper sx={{ width: "100%", mb: 2 }}>
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
                  <img src="images/grid.svg" alt="colvis" title="Show/Hide Columns"/>
                </span>
              </button>
            </div>
          </div>
        )}
        <TableContainer component={Paper} id="tableComp">
          <Table
            sx={{ minWidth: 650 }}
            aria-label="simple table"
            {...getTableProps()}
          >
            <TableHead>
              {headerGroups.map((headerGroup: any) => (
                <TableRow {...headerGroup.getHeaderGroupProps()}>
                  {headerGroup.headers.map((column: any, index: number) => (
                    <TableCell
                      {...column.getHeaderProps(column.getSortByToggleProps())}
                    >
                      <div key={index} className="col-header">
                        <span> {column.render("Header")}</span>

                        <span>
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
                        </span>
                      </div>
                    </TableCell>
                  ))}
                </TableRow>
              ))}
            </TableHead>
            <TableBody {...getTableBodyProps()}>
              <>
                {page.map((row: any, i) => {
                  prepareRow(row);
                  return (
                    <TableRow
                      sx={{ "&:last-child td, &:last-child th": { border: 0 } }}
                      {...row.getRowProps()}
                    >
                      {row.cells.map((cell: any) => {
                        return (
                          <TableCell {...cell.getCellProps()}>
                            {cell.render("Cell")}
                          </TableCell>
                        );
                      })}
                    </TableRow>
                  );
                })}
              </>
            </TableBody>
          </Table>
          {data.length === 0 && <div className="no-data">No data found</div>}
        </TableContainer>
        <TablePagination
          rowsPerPageOptions={[5, 10, 20]}
          component="div"
          count={data.length}
          rowsPerPage={rowsPerPage}
          page={pageNo}
          onPageChange={handleChangePage}
          onRowsPerPageChange={handleChangeRowsPerPage}
        />
      </Paper>
    </Box>
  );
};

export default TableComponent;
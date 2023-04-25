import React, { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import dayjs from "dayjs";
import utc from "dayjs/plugin/utc";
import tz from "dayjs/plugin/timezone";

import TableComponent from "components/Table/Table";
import { fetchData } from "api/api";
import { Subscription } from "../Subscription.interface";
import Toast from "components/Toast/Toast";

dayjs.extend(utc)
dayjs.extend(tz)

const SubscriptionHistory = () => {
    const navigate = useNavigate();
    const [data, setData] = useState<Array<Subscription>>([]);
    const [skipPageReset, setSkipPageReset] = useState(false);
    const [errorToast, setErrorToast] = useState<{display: boolean; statusText?: string; message?: string;}>({display: false});
    const timeZone = dayjs.tz.guess()

    useEffect(() => {
        fetchTableData();
    }, []);
    
    useEffect(() => {
        setSkipPageReset(false);
    }, [data]);

    const updateMyData = (rowIndex: any, columnID: any, value: any) => {
        setSkipPageReset(true); // turn on flag to not reset the page
        setData((old) =>
          old.map((row, index) => {
            if (index === rowIndex) {
              return {
                ...old[rowIndex],
                [columnID]: value,
              };
            }
            return row;
          })
        );
      };

    const fetchTableData = async () => {
        const result = await fetchData.get("/profile/current/subscriptions?just-enabled=false")
        if (result.data.length) {
          setData(result.data);
        }
    };

    const columns = React.useMemo(() => [
        {
            Header: "Type of Subscription",
            accessor: "name",
        },
        // {
        //     Header: "Amount Paid(in ADA)",
        //     accessor: "price",
        //     Cell: (props: any) => (
        //         <span>{(props.row.original.price / 1000000).toFixed(2)}</span>
        //     )
        // },
        {
            Header: "Amount Paid",
            accessor: "adaUsdPrice",
            Cell: (props: any) => (
                <span>${props.row.original.adaUsdPrice}</span>
            )
        },
        {
            Header: "Status",
            accessor: "status",
            Cell: (props: any) => {
                let text: string = '';
                if (props.row.original.status === 'active') {
                    text = 'Active'
                } else {
                  if (dayjs().isAfter(dayjs(props.row.original.endDate))) {
                    text = 'Expired'
                  } else if (props.row.original.status === 'inactive') {
                      text = 'Inactive'
                  } else if (props.row.original.status === 'pending') {
                      text = 'Pending'
                  }
                }
                return <span>{text}</span>
            }
        },
        {
            Header: "Date of Subscription",
            accessor: "startDate",
            Cell: (props: any) => (
              <span>
                {dayjs.utc(props.row.original.startDate).tz(timeZone).format("YYYY-MM-DD HH:mm:ss")}
              </span>
            ),
        },
        {
            Header: "Date of Expiry",
            accessor: "endDate",
            Cell: (props: any) => (
              <span>
                {dayjs.utc(props.row.original.endDate).tz(timeZone).format("YYYY-MM-DD HH:mm:ss")}
              </span>
            ),
        }
    ], [timeZone]);

    return (<>
      <button
        className="back-btn"
        onClick={(_) => {
          navigate(-1)
        }}
      >
        {" "}
        <img
          src="/images/back.png"
          alt="back_btn"
        />
      </button> 
      <div id="testHistory">
        <TableComponent dataSet={data} columns={columns}
          updateMyData={updateMyData}
          skipPageReset={skipPageReset}
        />
      </div>
      {(errorToast && errorToast.display) ? (
        ((errorToast.message && errorToast.statusText) ? 
        <Toast message={errorToast.message} title={errorToast.statusText}/> :
        <Toast />))
      : null}
    </>);
}

export default SubscriptionHistory;
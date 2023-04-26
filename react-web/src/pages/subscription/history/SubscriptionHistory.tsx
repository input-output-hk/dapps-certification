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
    const [errorToast, setErrorToast] = useState<{display: boolean; statusText?: string; message?: string;}>({display: false});
    const timeZone = dayjs.tz.guess()

    // to be invoked only once at first
    useEffect(() => {
      fetchTableData();
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, []);

    const fetchTableData = async () => {
      try {
        const result = await fetchData.get("/profile/current/subscriptions?just-enabled=false")
        if (result.data.length) {
          setData(result.data);
        }
      } catch (err) {
        handleError(err)
      }
    };

    const handleError = (error: any) => {
      if (error.response) {
        setErrorToast({display: true, statusText: error.response.statusText, message: error.response.data || undefined})
      } else {
        setErrorToast({display: true})
      }
      const timeout = setTimeout(() => { clearTimeout(timeout); setErrorToast({display: false}) }, 3000)
    }

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
                <span>${Math.round((props.row.original.price/1000000) * props.row.original.adaUsdPrice * 100) / 100}</span>
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
          updateMyData={() => {}}
          skipPageReset={false}
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
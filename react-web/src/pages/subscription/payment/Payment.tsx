import { BigNum } from "@emurgo/cardano-serialization-lib-browser";
import { fetchData } from "api/api";
import Button from "components/Button/Button";
import Modal from "components/Modal/Modal";
import Toast from "components/Toast/Toast";
import React, { useEffect, useState } from "react";
import { useDispatch } from "react-redux";
import { useLocation, useNavigate } from "react-router-dom";
import { payFromWallet } from "store/slices/walletTransaction.slice";
import { useAppSelector } from "store/store";
import "./Payment.scss";
import dayjs from "dayjs";
import { Subscription } from "../Subscription.interface";


function Payment() {
  const { state } = useLocation();
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const { address, wallet } = useAppSelector((state) => state.auth);
  const { error } = useAppSelector((state) => state.walletTransaction);
  const [transactionId, setTransactionId] = useState("");
  const [showError, setShowError] = useState("");
  const [openModal, setOpenModal] = useState(false);
  const [processing, setProcessing] = useState(false);
  const zeroVal: number = 0;
  let currentTierPrice: BigNum = BigNum.from_str(zeroVal.toString());
  let currentSubscriptionId: string = '';

  const onCloseModal = () => { 
    setOpenModal(false)
    navigate(-1)
  }

  const handleError = (errorObj: any) => {
    let errorMsg = ''
    if (typeof errorObj === 'string') {
        errorMsg = errorObj + ' Please try again.'
    } else if (errorObj?.info) {
        errorMsg = errorObj.info + ' Please try again.'
    } else if (errorObj?.response?.message) {
        errorMsg = errorObj?.response.message + ' Please try again.'
    } else if (errorObj?.response?.data) {
        errorMsg = errorObj.response.statusText + ' - ' + errorObj.response.data 
    } else {
      errorMsg = 'Something wrong occurred. Please try again later.'
    }
    setShowError(errorMsg);
    setProcessing(false)
    const timeout = setTimeout(() => { 
      clearTimeout(timeout); 
      setShowError("") 
    }, 5000)
    setOpenModal(false)
    console.log(errorMsg)
}

  useEffect(() => {
    error && handleError(error)
  }, [error])

  const triggerPayment = async () => {
    setShowError("");
    setProcessing(true);
    try {
      const response: {data: BigNum} = await fetchData.get("/profile/current/balance")
      if (response.data) {
        const availableProfileBalance: BigNum = BigNum.from_str(response.data.toString());
        let lessBalance = false
        if (availableProfileBalance.less_than(currentTierPrice)) {
          lessBalance = true;
        } else {
          const difference = availableProfileBalance.checked_sub(currentTierPrice)
          lessBalance = difference.less_than(BigNum.from_str(zeroVal.toString()))
        }
        if (lessBalance) {
          const oneAdaInLovelaces = 1000000;
          const min_fee: BigNum = BigNum.from_str(oneAdaInLovelaces.toString()); // 1 ADA in lovelaces - min req for cardano wallet txn
          let fee: BigNum = min_fee;
          if (min_fee.less_than(currentTierPrice)) {
            fee = currentTierPrice;
          }
          triggerTransactionFromWallet(fee);
        } else {
          fetchCurrentSubscription()
        }
      }
    } catch (err) {
      handleError(err)
    }
  };

  const triggerTransactionFromWallet = async (fee_in_lovelace: BigNum) => {
    const response = await dispatch(payFromWallet({fee: fee_in_lovelace, wallet: wallet, address: address}))
    if (response.payload) {
      setTransactionId(response.payload);
      fetchCurrentSubscription(true)
    } else if (response?.error?.message) {
      handleError(response.error.message);
    }
  }

  const fetchCurrentSubscription = (isAfterPayment?: boolean) => {
    fetchData.get("/profile/current/subscriptions").then((response: {data: Subscription[]}) => {
      const current: Subscription | undefined = response.data.find((item: Subscription) => item.id === currentSubscriptionId) 
      if (current && current.tierId === state.id) {
        if (current.status === 'pending') {
          if (isAfterPayment) {
            // keep calling this GET in a 1 sec delay to check the status
            const timeout = setTimeout(() => {
              clearTimeout(timeout)
              fetchCurrentSubscription(isAfterPayment)
            }, 1000)
          } else {
            currentTierPrice = BigNum.from_str(current.price.toString());
            triggerPayment()
          }
        } else if (current.status === 'active' && !dayjs().isAfter(dayjs(current.endDate))) {
          // payment retrieved from balance
          setProcessing(false);
          setOpenModal(true);
        } 
      }
    }).catch(handleError);
  }

  const postSubscription = () => {
    setShowError("");
    setProcessing(true);
    fetchData
      .post("/profile/current/subscriptions/" + state.id)
      .then((response: {data: Subscription}) => {
        currentSubscriptionId = response.data.id
        fetchCurrentSubscription()
      }).catch(handleError);
  }

  const renderPage = () => {
    return (
    <div className="payment-container">
      <div className="content">
        <h4>{state.type}</h4>
        <h3>{state.name}</h3>
        <h5>{state.subtitle}</h5>
        <p>{state.description}</p>
      </div>
      <div className="btn-layout">
        <Button
          buttonLabel={"Cancel"}
          onClick={() => navigate(-1)}
          className="cancel"
          displayStyle="primary-outline"
          disabled={processing}
        ></Button>
        <Button
          buttonLabel={"Pay $" + state.usdPrice}
          onClick={() => postSubscription()}
          className="pay"
          displayStyle="primary"
          showLoader={processing}
        ></Button>
      </div>
      <Modal
        open={openModal}
        title="Subscription Requested"
        onCloseModal={onCloseModal}
        modalId="subscriptionSuccessModal"
      >
        <p style={{ marginBottom: "2rem" }}>
          Successfully initiated subscription of {state.name}
        </p>
        {transactionId ? (<span>
          View your performed payment transaction&nbsp;
          <a
            target="_blank"
            rel="noreferrer"
            href={`https://preprod.cardanoscan.io/transaction/${transactionId}`}
          >
            here
          </a>
          !
        </span>) : null}
      </Modal>
      {showError ? <Toast message={showError} /> : null}
    </div>
    );
  }

  return state ? renderPage() : null
}

export default Payment;
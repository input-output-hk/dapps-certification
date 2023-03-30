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


function Payment() {
  const { state } = useLocation();
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const { address, wallet } = useAppSelector((state) => state.auth);
  const { error } = useAppSelector(state => state.walletTransaction);
  const [transactionId, setTransactionId] = useState("")
  const [ showError, setShowError ] = useState("");
  const [ openModal, setOpenModal ] = useState(false);

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
    setShowError("")
    fetchData.get('/profile/current/balance').then(response => {
      const availableProfileBalance: number = response.data
      // fetchData.get().then(res => {
          // const runDetails: Run = res.data
          if ((availableProfileBalance - state.lovelace_price) < 0) {
              triggerTransactionFromWallet(state.lovelace_price)
          } else {
              initiatePurchase()
          }
      // })
    })
  }
  
  const triggerTransactionFromWallet = async (fee_in_lovelace: BigNum) => {
    const response = await dispatch(payFromWallet({fee: fee_in_lovelace, wallet: wallet, address: address}))
    if (response.payload) {
      setTransactionId(response.payload)
      initiatePurchase()
    }
  }

  const initiatePurchase = () => {
    fetchData.post('/profile/current/subscriptions/' + state.tier_id)
      .then((response: any) => {
        setOpenModal(true);
      }).catch(handleError)
  }

  return (
    <div className="payment-container">
      <div className="content">
        <h4>{state.type}</h4>
        <h3>{state.tier_name}</h3>
        <h5>{state.featureSet}</h5>
        <p>{state.description}</p>
      </div>
      <div className="btn-layout">
        <Button buttonLabel={"Cancel"} onClick={() => navigate(-1)} className="cancel" displayStyle="primary-outline"></Button>
        <Button buttonLabel={"Pay $" + state.usd_price} onClick={() => triggerPayment()} className="pay" displayStyle="primary"></Button>
      </div>
      <Modal open={openModal} title="Subscription Requested" onCloseModal={onCloseModal}>
        <p style={{marginBottom: '2rem'}}>Successfully initiated subscription of {state.tier_name}</p>
        <span>
            View your performed payment transaction&nbsp;
            <a target="_blank" rel="noreferrer" href={`https://preprod.cardanoscan.io/transaction/${transactionId}`}>here</a>!
        </span>
      </Modal>
      {showError ? <Toast message={showError} /> : null}
    </div>
  );
}

export default Payment;
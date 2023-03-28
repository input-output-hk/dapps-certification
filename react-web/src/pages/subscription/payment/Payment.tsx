import { BigNum } from "@emurgo/cardano-serialization-lib-browser";
import { fetchData } from "api/api";
import Modal from "components/Modal/Modal";
import React, { useEffect, useState } from "react";
import { useDispatch } from "react-redux";
import { useLocation, useNavigate } from "react-router-dom";
import { handleError, payFromWallet } from "store/slices/walletTransaction.slice";
import { useAppSelector } from "store/store";
import "./Payment.scss";


function Payment() {
  const { state } = useLocation();
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const { address, wallet } = useAppSelector((state) => state.auth);
  const { error } = useAppSelector(state => state.walletTransaction);
  const [transactionId, setTransactionId] = useState("")
  const [subscriptionSuccess, setSubscriptionSuccess] = useState(false)
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
    }
    // setShowError(errorMsg.length > 50 ? 'Something wrong occurred. Please try again later.' : errorMsg);
    const timeout = setTimeout(() => { 
      clearTimeout(timeout); 
      // setShowError("") 
    }, 5000)
    // setCertifying(false);
    if (errorObj?.response?.status === 403) {
        // setDisableCertify(true)
    }
    console.log(errorMsg)
}

  useEffect(() => {
    handleError(error)
  }, [error])

  const triggerPayment = async () => {
    fetchData.get('/profile/current/balance').then(response => {
      const availableProfileBalance: number = response.data
      // fetchData.get().then(res => {
          // const runDetails: Run = res.data
          // if ((availableProfileBalance - state.lovelace) < 0) {
              triggerTransactionFromWallet(state.lovelace)
          // } else {
          //     initiatePurchase()
          // }
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
    fetchData.post('/profile/current/subscriptions/' + state.tierId)
      .catch(handleError)
      .then((response: any) => {
        setSubscriptionSuccess(true)
      })
  }

  return (
    <div className="payment-container">
      <div className="content">
        <h4>{state.type}</h4>
        <h3>{state.name}</h3>
        <h5>{state.featureSet}</h5>
        <p>{state.description}</p>
      </div>
      <div className="btn-layout">
        <button onClick={() => navigate(-1)} className="cancel button-pay">
          Cancel
        </button>
        <button className="pay button-pay" onClick={() => triggerPayment()}>Pay ${state.price}</button>
      </div>
      {subscriptionSuccess ? 
        <Modal open={openModal} title="Certification Successful" onCloseModal={onCloseModal}>
          <p>Successfully initiated subscription of {state.name}</p>
          <span>
              View your performed payment transaction &nbsp;
              <a target="_blank" rel="noreferrer" href={`https://preprod.cardanoscan.io/transaction/${transactionId}`}>here</a>!
          </span>
        </Modal>
      : null}
    </div>
  );
}

export default Payment;
import React from "react";
import "./PricingCard.scss";
import { useNavigate } from "react-router-dom";


const PricingCard: React.FC<any> = ({ id, name, enabled, featureSet, description, lovelace, price, type }) => {

  const navigate = useNavigate();
  const onPayment = (e: any) => {
    navigate("/subscription/payment", { state: { id, name, featureSet, description, lovelace, price, type } });
  };

  const contactUs = () => {
    //do the contact us logic here
  }

  return (
    <div className={`card_content clearfix ${enabled? '' : 'disabled'}`}>
      <div className={`${price ? 'card_head_price' : ''} card_head clearfix`}>
        <div className="card_head_content clearfix">
          <div className="head_bg"></div>
          <div className="head">
            <span>{name}</span>
          </div>
        </div>

        {price ? <>
          <div className="card_price_tag clearfix">
            <span className="price">
              <span className="sign">$</span>
              <span className="currency">{price}</span>
            </span>          
          </div>
        </> : null}
        
      </div>

      <div className="card_feature_list">
        <ul>
          <li>{featureSet}</li>
          {/* <li>
            <span>150GB</span> Storage
          </li> */}
        </ul>
      </div>

      <div className={`card_price_btn clearfix ${price ? '' : 'card_price_btn_fixed'}`}>
        {price ? 
        <button
          className="button-pay"
          onClick={(e) => {
            onPayment(e);
          }}
        >
          Next
        </button> : <button
          className="button-pay"
          onClick={(e) => {
            contactUs()
          }}
        >
          Contact Us
        </button>}
      </div>
    </div>
  );
};

export default PricingCard;
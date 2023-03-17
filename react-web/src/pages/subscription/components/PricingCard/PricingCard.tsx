import React from "react";
import "./PricingCard.scss";

const PricingCard: React.FC<any> = ({ name, disabled, featureSet, description, price }) => {
  return (
    <div className={`card_content clearfix ${disabled? 'disabled' : ''}`}>
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
        <button
          className="button-pay"
          onClick={(e) => {
            // onPayment(e);
          }}
        >
          Next
        </button>
      </div>
    </div>
  );
};

export default PricingCard;
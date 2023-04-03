import React, { useEffect, useState } from "react";
import "./PricingCard.scss";
import { useNavigate } from "react-router-dom";
import Button from "components/Button/Button";
import Modal from "components/Modal/Modal";
import { Input } from "compositions/Form/components/Input";
import { Form } from "compositions/Form/Form";
import { contactFormSchema } from "./contactForm.schema";
import { useForm } from "hooks/useForm";
import { postExternal } from "api/api";


const PricingCard: React.FC<any> = ( {...props} ) => {
  const navigate = useNavigate();
  const onPayment = (e: any) => {
    navigate("/subscription/payment", { state: { ...props } });
  };

  const [showContactUs, setShowContactUs] = useState(false)
  const [contactFormSubmitted, setContactFormSubmitted] = useState(false)
  const contactUs = () => {
    //do the contact us logic here
    form.setValue("subject", "Subscribe to " + props.name + " Subscription - " + props.tier_name)
    setShowContactUs(true)
  }
  const onCloseModal = () => {
    setShowContactUs(false)
  }
  const form: any = useForm({
    schema: contactFormSchema,
    mode: "onChange",
  });
  const formHandler = (formData: any) => {
    postExternal.post('https://iog-io-contact-form.vercel.app/api/contact', formData).then(res => {
        setContactFormSubmitted(true)
    })
  }
  return (
    <>
    <div className={`card_content clearfix ${props.enabled? '' : 'disabled'}`}>
      <div className={`${props.usdPrice ? 'card_head_price' : ''} card_head clearfix`}>
        <div className="card_head_content clearfix">
          <div className="head_bg"></div>
          <div className="head">
            <span>{props.tier_name}</span>
          </div>
        </div>

        {props.usdPrice ? <>
          <div className="card_price_tag clearfix">
            <span className="price">
              <span className="sign">$</span>
              <span className="currency">{props.usdPrice}</span>
            </span>          
          </div>
        </> : null}
        
      </div>

      <div className="card_feature_list">
        <ul>
          <li>{props.featureSet}</li>
          {/* <li>
            <span>150GB</span> Storage
          </li> */}
        </ul>
      </div>

      <div className={`card_price_btn clearfix ${props.usdPrice ? '' : 'card_price_btn_fixed'}`}>
        {props.usdPrice ? 
          props.status === 'pending' || props.status === 'active'  ? 
            <Button displayStyle="primary-outline" buttonLabel={props.status.toUpperCase()} disabled></Button>
          :
            <button className="button-pay" onClick={(e) => {onPayment(e);}}>Next</button> 
        : <button className="button-pay" onClick={(e) => {contactUs()}}>Contact Us</button>
        }
      </div>
    </div>
    <Modal open={showContactUs} title="Contact Us" onCloseModal={onCloseModal}>
      {contactFormSubmitted ? 
        <h5>Thank you for reaching out to us. <br/>We'll get in touch with you soon.</h5>
      : <Form form={form} onSubmit={formHandler}>
          <Input label="Name" id="name" name="name" required {...form.register("name")}></Input>
          <Input label="Email" type="email" id="email" name="email" required {...form.register("email")}></Input>
          <Input label="Subject" id="subject" name="subject" required disabled={true} {...form.register("subject")} disablefocus="true"></Input>
          <Input label="Message" id="message" name="message" {...form.register("message")}></Input>
          <Button buttonLabel={"Send"} disabled={!form.formState.isValid} type="submit"></Button>
        </Form>
      }
    </Modal>
    </>
  );
};

export default PricingCard;
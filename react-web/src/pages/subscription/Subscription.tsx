import React from "react";
import PricingCard from "./components/PricingCard/PricingCard";
import "./Subscription.scss";

const developerTiers = [
  {
    name: "Tier I",
    featureSet: "Minimal Features",
    price: '100',
    description:
      "This tier is perfect for developers who are just starting out with securing their software. With this tier, you will have access to our basic features, which will help you get a L1 certificate.",
  },
  {
    name: "Tier II",
    featureSet: "All Features",
    price: '1000',
    description:
      "This tier is perfect for developers who want full access to all of our features. With this tier, you will have access to our full suite of tools, which will help you ensure that your software is fully compliant with industry standards.",
  },
  {
    name: "Tier III ",
    featureSet: "All Features + Professional Services",
    price: '',
    description:
      "This tier is perfect for developers who want a fully customized experience. With this tier, you will have access to all of our features, as well as our team of professional services experts who can help you ensure that your software is fully compliant and secure.",
  },
];
const auditorTiers = [
  {
    name: "Tier I",
    featureSet: "All Features Enabled",
    price: '1000',
    description:
      "This tier is perfect for auditors who want full access to all of our features. With this tier, you will have access to our full suite of tools, which will help you ensure that your clients' software is fully compliant with industry standards.",
  },
  {
    name: "Tier II",
    featureSet: "All Features Enabled + Professional Services",
    price: '',
    description:
      "This tier is perfect for auditors who want a fully customized experience. With this tier, you will have access to all of our features, as well as our team of professional services experts who can help you ensure that your clients' software is fully compliant and secure.",
  },
];

const Subscription = () => {
  return (
    <div className="pricing-container">
      <div className="subscription-content">
        Welcome to our subscription offering page for our SaaS tool! We offer
        two types of subscriptions: one for developers and one for auditors. Our
        developer subscription has three tiers, while our auditor subscription
        has two tiers.
      </div>
      <div className="tier-container">
        <h5>Developer Subscription</h5>
        <p>
          Our developer subscription is perfect for developers who want to
          ensure that their software is secure and compliant with industry
          standards. Here are the three tiers we offer:
        </p>
        <div className="pricing-card-container">
          {developerTiers.map((item, id) => (
            <PricingCard key={id} {...item} />
          ))}
        </div>
      </div>
      <div className="tier-container">
        <h5>Auditor Subscription</h5>
        <p>
          Our auditor subscription is perfect for auditors who want to ensure
          that their clients' software is secure and compliant with industry
          standards. Here are the two tiers we offer:
        </p>
        <div className="pricing-card-container">
          {auditorTiers.map((item, id) => (
            <PricingCard key={id} {...item} />
          ))}
        </div>
      </div>
    </div>
  );
};

export default Subscription;
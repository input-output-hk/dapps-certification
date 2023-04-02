import React, { useEffect, useState } from 'react';
import { fetchData } from 'api/api';
import PricingCard from './components/PricingCard/PricingCard';
import { Subscription, Tier } from './Subscription.interface';

const SubscriptionContent = () => {
  const [developerTiers, setDeveloperTiers] = useState([
      {
        id: '1',
        tierId: '1',
        tier_name: "Tier I",
        featureSet: "Minimal Features to get L1 certificate",
        price: '100',
        lovelace: 1000000,
        enabled: true,
        description:
          "This tier is perfect for developers who are just starting out with securing their software. With this tier, you will have access to our basic features, which will help you get a L1 certificate.",
      },
      {
        id: '2', 
        tierId: '2',
        tier_name: "Tier II",
        featureSet: "All Features and customizations",
        price: '1000',
        lovelace: 3000000,
        description:
          "This tier is perfect for developers who want full access to all of our features. With this tier, you will have access to our full suite of tools, which will help you ensure that your software is fully compliant with industry standards.",
      },
      // {
      //   id: 3,
      //   tier_name: "Tier III ",
      //   featureSet: "All Features and customizations + IOG's Professional Services",
      //   price: '',
      //   description:
      //     "This tier is perfect for developers who want a fully customized experience. With this tier, you will have access to all of our features, as well as our team of professional services experts who can help you ensure that your software is fully compliant and secure.",
      // }
  ]);
  const [auditorTiers, setAuditorTiers] = useState([
    {
      id: '1',
      tierId: '1',
      tier_name: "Tier I",
      featureSet: "All Features and customizations + upload of a report on chain",
      price: '1000',
      lovelace: 4000000,
      description:
        "This tier is perfect for auditors who want full access to all of our features. With this tier, you will have access to our full suite of tools, which will help you ensure that your clients' software is fully compliant with industry standards.",
    },
    {
      id: '2',
      tierId: '2',
      tier_name: "Tier II",
      featureSet: "All Features and customizations + upload of a report on chain + IOG's Professional Services",
      price: '',
      description:
        "This tier is perfect for auditors who want a fully customized experience. With this tier, you will have access to all of our features, as well as our team of professional services experts who can help you ensure that your clients' software is fully compliant and secure.",
    },
  ]);

  const [adaUsdPrice, setAdaUsdPrice] = useState(0)

  useEffect(() => {
    fetchCurrentUsdPrice()
    fetchAllTiers()
    fetchActiveSubscription();
  }, []);

  const fetchCurrentUsdPrice = () => {
    const response: any = fetchData.get('/ada-usd-price')
    if (response.data) {
      setAdaUsdPrice(response.data)
    }
  }

  const modifyTierData = (item: Tier, tier: any) => {
    if ((tier.tierId || '1') === (item.tierId || '1')) {
      tier = {...tier, ...item}
      tier['ada_price'] = tier.usdPrice * adaUsdPrice
      tier['lovelace_price'] = tier.ada_price * 1000000
      return tier
    } else {
      return null
    }
  }

  const fetchAllTiers = async () => {
    const response: any = await fetchData.get('/tiers')
    const result: Array<Tier> = response.data
    if (result.length) {
      // merge with our dev/auditor data set
      result.forEach((item: Tier, idx: number) => {
        if (item.name === 'Developer') {
          setDeveloperTiers(developerTiers.map((tier: any) => modifyTierData(tier, item)))
        } else if (item.name === 'Auditor') {
          setAuditorTiers(auditorTiers.map((tier: any) => modifyTierData(tier,item)))
        }
      })
    }
  }

  const fetchActiveSubscription = async () => {
    const response: any = await fetchData.get("/profile/current/subscriptions?just-enabled=true")
    const result: Array<Subscription> = response.data
    if (result.length === 1 && result[0].hasOwnProperty('tierId')) {
      // set active to the merged data set
      if (result[0].name === 'Developer') {
        setDeveloperTiers(developerTiers.map((tier: any) => modifyTierData(tier,result[0])))
      } else if (result[0].name === 'Auditor') {
        setAuditorTiers(auditorTiers.map((tier: any) => modifyTierData(tier,result[0])))
      }
    }
  }

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
        {developerTiers.map((item, id) => item ? (<PricingCard key={id} {...item} type="Developer Subscription"/>) : null)}
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
        {auditorTiers.map((item, id) => item ? (<PricingCard key={id} {...item} type="Auditor Subscription" />) : null)}
      </div>
    </div>
    <div className="subscription-content">Our pricing varies depending on the tier you choose. Please contact us for more information.</div>
    <div className="subscription-content">
    Thank you for considering our subscription offering for our SaaS tool. We are confident that our tools and professional services will help you ensure that your software is secure and compliant with industry standards. If you have any questions or would like to sign up for a subscription, please contact us.
    </div>
  </div>
  )
}

export default SubscriptionContent;
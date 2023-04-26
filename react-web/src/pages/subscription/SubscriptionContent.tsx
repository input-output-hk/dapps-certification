import { useMemo, useEffect, useState } from 'react';
import Alert from '@mui/material/Alert';
import { fetchData } from 'api/api';
import PricingCard, {PriceCardProps} from './components/PricingCard/PricingCard';
import { Subscription, Tier } from './Subscription.interface';
import { useNavigate } from 'react-router-dom';
import Button from 'components/Button/Button';

type TiersByType = {
  developers: PriceCardProps[];
  auditors: PriceCardProps[];
}

// a hook to split the tiers by type
const useTiers = (): TiersByType => {
  const [tiers, setTiers] = useState([] as Tier[])
  const [subscriptions, setSubscriptions] = useState([] as Subscription[])
  // fetch tiers and subscriptions
  useEffect(() => {
    (async() => {
      setSubscriptions((await fetchData.get('/profile/current/subscriptions?just-enabled=true')).data)
      setTiers((await fetchData.get('/tiers')).data)
    })()
  }, []);

  // return a memoized object with the tiers split by type
  return useMemo(() => {
    // check if there is an active subscription
    const hasActive = subscriptions.some(({status,endDate}: Subscription) =>
      status === 'active' && new Date(endDate) >= new Date())

    // a function to map the tier to the card props
    const toProps = (tier: Tier): PriceCardProps => {
      // find the subscription for the tier
      const subscription = subscriptions.find(({tierId,status,endDate}: Subscription) =>
        tierId === tier.id && status === 'active' && new Date(endDate) >= new Date())
      return {
        ...tier,
        enabled: !hasActive,
        status: subscription ? subscription.status : undefined
      }
    }
    // map tiers to card props
    const cardProps = tiers.map(toProps)

    // split the tiers by type
    return {
      developers: cardProps.filter((tier: Tier) => tier.type === 'developer') ,
      auditors: cardProps.filter((tier: Tier) => tier.type === 'auditor')
    }
  },[tiers,subscriptions])
}

// a component to render the list of cards
const CardList: React.FC<{cards: PriceCardProps[]}> = ({cards}) => {
  return cards.length
    ? <> {cards.map((item) => <PricingCard key={item.id} {...item} />)} </>
    : <span className="empty-card"><Alert severity="warning">Unable to load data. Please try again later.</Alert></span>;
}

const SubscriptionContent = () => {
  const navigate = useNavigate();
  const {developers,auditors} = useTiers()

  return (
    <>
      <div className="head-section">
      <label id="view-subscription-history">
        <Button
          displayStyle="primary" 
          onClick={() => navigate('/subscription/history')}
          iconUrl="images/list.svg" buttonLabel={"View History"}></Button>
      </label>
      </div>
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
            <CardList cards={developers} />
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
            <CardList cards={auditors} />
          </div>
        </div>
        <div className="subscription-content">Our pricing varies depending on the tier you choose. Please contact us for more information.</div>
        <div className="subscription-content">
          Thank you for considering our subscription offering for our SaaS tool. We are confident that our tools and professional services will help you ensure that your software is secure and compliant with industry standards. If you have any questions or would like to sign up for a subscription, please contact us.
        </div>
      </div>
    </>
  )
}

export default SubscriptionContent;

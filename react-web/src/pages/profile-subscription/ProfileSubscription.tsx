import * as React from "react";
import Stack from "@mui/material/Stack";
import Box from "@mui/material/Box";
import Card from "./components/PlanCard/Card";
import { Typography } from "@mui/material";
import DevPlanForm from "./components/Forms/DevPlanForm";
import AuditorPlanForm from "./components/Forms/AuditorPlanForm";

const ProfileSubscription = () => {
  const [formIndex, setForm] = React.useState<any>();

  return (
    <Box sx={{ maxWidth: "100%" }}>
      <div style={{ marginBottom: "32px", textAlign: "center" }}>
        <Typography variant="h5" sx={{ mb: 2 }}>
          DApp Shield
        </Typography>
        <p>Please choose the Package you wish to subscribe to.</p>
      </div>

      <Stack
        spacing={{ xs: 1, sm: 2 }}
        direction={{ xs: "column", sm: "row" }}
        flexWrap="wrap"
        justifyContent="center"
      >
        <div data-testid="dev-plan" onClick={() => setForm(0)}>
          <Card
            isSelected={formIndex === 0}
            buttonText={{
              baseText: "SELECT",
              selectedText: "SELECTED",
            }}
          >
            <Typography variant="h5" sx={{ mb: 2 }}>
              Developer Premium Tier
            </Typography>
            <Typography variant="h4" sx={{ mb: "5rem" }}>
              $999
            </Typography>
            <Typography variant="h6">Limited testing features</Typography>
            <p>Limited number of checks</p>
            <p>Limited number of properties</p>
            <p>Low priority queue</p>
          </Card>
        </div>

        <div data-testid="auditor-plan" onClick={() => setForm(1)}>
          <Card
            isSelected={formIndex === 1}
            buttonText={{
              baseText: "SELECT",
              selectedText: "SELECTED",
            }}
          >
            <Typography variant="h5" sx={{ mb: 2 }}>
              Auditor Premium Tier
            </Typography>
            <Typography variant="h4" sx={{ mb: "5rem" }}>
              $9,999
            </Typography>
            <Typography variant="h6">Unlimited testing features</Typography>
            <p>Unlimited number of checks</p>
            <p>Unlimited number of properties</p>
            <p>High priority queue</p>
          </Card>
        </div>
      </Stack>

      <div
        data-testid="plan-forms"
        style={{ marginTop: "32px" }}
      >
        {formIndex === 0 ? <DevPlanForm /> : null}
        {formIndex === 1 ? <AuditorPlanForm /> : null}
      </div>
    </Box>
  );
};

export default ProfileSubscription;
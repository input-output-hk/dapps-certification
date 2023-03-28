import { combineReducers } from "@reduxjs/toolkit";

import authReducer from "./slices/auth.slice";
import certificationReducer from "pages/certification/slices/certification.slice";
import deleteTestHistory from "pages/testHistory/slices/deleteTestHistory.slice";
import logRunTimeSlice from "pages/certification/slices/logRunTime.slice";
import walletTransactionSlice from "./slices/walletTransaction.slice";

const rootReducer = combineReducers({
  auth: authReducer,
  certification: certificationReducer,
  deleteTestHistory,
  runTime: logRunTimeSlice,
  walletTransaction: walletTransactionSlice
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;
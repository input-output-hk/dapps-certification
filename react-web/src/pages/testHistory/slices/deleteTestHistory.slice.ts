import { createSlice, createAsyncThunk } from "@reduxjs/toolkit";
import { fetchData } from "api/api";

const initialState = {
  loading: false,
  msg: null,
};

export const deleteTestHistoryData = createAsyncThunk(
  "deleteAttraction",
  async (payload: any, { rejectWithValue }) => {
    try {
      const response = await fetchData.delete(payload.url);
      return response.data;
    } catch (e: any) {
      return rejectWithValue(e.response.data);
    }
  }
);

const deleteTestHistoryDataSlice = createSlice({
  name: "deleteTestHistoryData",
  initialState: initialState,
  reducers: {},
  extraReducers: (builder) => {
    builder
      .addCase(deleteTestHistoryData.rejected, (state) => {
        state.loading = false;
      })
      .addCase(deleteTestHistoryData.pending, (state) => {
        state.loading = false;
      })
      .addCase(deleteTestHistoryData.fulfilled, (state) => {
        state.loading = false;
      });
  },
});

export default deleteTestHistoryDataSlice.reducer;
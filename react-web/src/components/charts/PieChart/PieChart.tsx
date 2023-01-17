import React from "react";
import { Chart } from "react-google-charts";

const PieChart: React.FC<{
    title: string;
    is3D: boolean;
    data: {data: any, totalCount: number};
}> = ({title, is3D, data}) => {
    const chartData: any = data.data;
    const totalCountStr = data.totalCount ? ' (' + data.totalCount + ' in total)' : '';
    const options = {
        title: title + totalCountStr,
        is3D: is3D,
        backgroundColor: 'transparent',
        colors: ['#657CD0','#DA68A0','#06C3C0','#777B80','#7C6D70','#7C0850','#F75870'],
        legend: {position: 'none'},
        titleTextStyle: {
            color: '#383838'
        }
    }

    return (
        <Chart 
            chartType="PieChart"
            data={chartData}
            options={options}
            width={"100%"}
        />
    )
};

export default PieChart;
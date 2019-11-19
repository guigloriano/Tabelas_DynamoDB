let date = "20191118"

const AWSConfig = require('./config')
const fs = require('fs')
const csv = require('fast-csv')
const tableDefiner = require('./define-table')
const dateFormater = require('./format-date')
const ws = fs.createWriteStream(`inversor_1_ufms-${date}.csv`)
const docClient = AWSConfig.docClient;

const requireAWSData = async (params) => {
	return new Promise((resolve, reject) => {

		let items = []
		let types = []
		let interval = []

		let pot_AC = []
		let current_AC = []
		let current_DC = []
		let voltage_AC = []
		let voltage_DC = []
		let irr_inverter = []
		let irradiationInterval = []
		

		docClient.query(params, (err, data) => {
			if (err) {
				reject("Unable to scan table. Error JSON: " + JSON.stringify(err, null, 2))
			}
			else {
				let qtd = 0

				data.Items.forEach(function (item) {
					if (typeof data.Items != "undefined") {

						let formatedDate = dateFormater.formatDate(item.dia_mes_ano, item.hora_minuto)

						let type = item.tipo

						let P_AC = item.P_AC
						let I_AC = item.I_AC
						let I_DC = item.I_DC
						let V_AC = item.V_AC
						let V_DC = item.V_DC
						let IRR = (item.IRR)

						items.push({
							date: formatedDate.hourMin,
							P_AC: P_AC || 0,
							I_AC: I_AC || 0,
							I_DC: I_DC || 0,
							V_AC: V_AC || 0,
							V_DC: V_DC || 0,
							IRR: IRR || 0,
							type: type || "null",

						})

						interval.push(formatedDate.hourMin)
						qtd++

						if (item.hora_minuto >= 60000 && item.hora_minuto <= 190000) {
							irradiationInterval.push(formatedDate.hourMin)
						}

					}

				})

				interval.sort()
				irradiationInterval.sort()

				for (let hour of interval) {
					for (let item of items) {
						if (hour == item.date) {
							pot_AC.push(item.P_AC)
							current_AC.push(item.I_AC)
							current_DC.push(item.I_DC)
							voltage_AC.push(item.V_AC)
							voltage_DC.push(item.V_DC)
							irr_inverter.push(item.IRR)
						}
					}
				}

			}

			resolve([
				interval,
				pot_AC,
				current_AC,
				current_DC,
				voltage_AC,
				voltage_DC,
				irr_inverter,
				types
			])
		})
	})
}

const readForOneDay = async (date) => {

	let dateToRequest = {
		day:
			date[6] +
			date[7],
		month:
			date[4] +
			date[5],
		year:
			date[0] +
			date[1] +
			date[2] +
			date[3]
	}

	return new Promise((resolve, reject) => {

		let params = tableDefiner.defineTable(
			'campo-grande',
			'production',
			null,
			dateToRequest.day,
			dateToRequest.month,
			dateToRequest.year,
			null
		)

		requireAWSData(params)
			.then((response) => {

				let items = {
			
					interval: response[0],
					pot_AC: response[1],
					current_AC: response[2],
					current_DC: response[3],
					voltage_AC: response[4],
					voltage_DC: response[5],
					irr_inverter: response[6],
					day: dateToRequest.day,
					month: dateToRequest.month,
					year: dateToRequest.year,
					monthDay: dateToRequest.year +  dateToRequest.month + dateToRequest.day,
				}

				resolve(items)
			})
			.catch(err => {
				console.log(err)
			})

	})

}

readForOneDay(date)
	.then((response) => {
		
		let arrayToWrite = []
		
		arrayToWrite.push([
			'dia_mes_ano',
			'hora_minuto',
			'P_AC',
			'I_AC',
			'I_DC',
			'V_AC',
			'V_DC',
			'IRR',
		])

		for (let i = 0; i < response.interval.length; i++) {
			arrayToWrite.push([
				response.monthDay,
				response.interval[i],
				response.pot_AC[i], 
				response.current_AC[i],
				response.current_DC[i],
				response.voltage_AC[i],
				response.voltage_DC[i],
				response.irr_inverter[i],
			])

			console.log("inserindo: " + response.interval[i])
		}
		
		csv.write(arrayToWrite, {headers: true}).pipe(ws)

	})
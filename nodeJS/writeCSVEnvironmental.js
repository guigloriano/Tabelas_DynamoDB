let date = "20200322"

const AWSConfig = require('./config')
const fs = require('fs')
const csv = require('fast-csv')
const tableDefiner = require('./define-table')
const dateFormater = require('./format-date')
//const windDirectionConverter = require('./wind-direction-converter')
const ws = fs.createWriteStream(`ambientais-ufms-${date}.csv`)
const docClient = AWSConfig.docClient;

const requireAWSData = async (params) => {
	return new Promise((resolve, reject) => {

		let items = []
		let types = []
		let interval = []
		let humidities = []
		let PM1Numbers = []
		let PM2Numbers = []
		let PM4Numbers = []
		let windSpeeds = []
		let PM10Numbers = []
		let averageSizes = []
		let temperatures = []
		let windDirections = []
		let PM1Particulates = []
		let PM2Particulates = []
		let PM4Particulates = []
		let PM10Particulates = []
		let irradiations = []		
		let irradiationInterval = []
		let rainfall = []


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
						let numPM1 = item.numPM1
						let numPM2 = item.numPM2
						let numPM4 = item.numPM4
						let numPM10 = item.numPM10
						let temperature = item.temp
						let windDir = item.vento_dir
						let massaPM1 = item.massaPM1
						let massaPM2 = item.massaPM2
						let massaPM4 = item.massaPM4
						let massaPM10 = item.massaPM10
						let windSpeed = item.vento_vel
						let averageSize = item.tamanho_medio
						let humidity = item.hum
						let irradiation = item.irr
						let rainfall = item.rainfall

						items.push({
							date: formatedDate.hourMin,
							massaPM1: massaPM1 || 0,
							massaPM2: massaPM2 || 0,
							massaPM4: massaPM4 || 0,
							massaPM10: massaPM10 || 0,
							numPM1: numPM1 || 0,
							numPM2: numPM2 || 0,
							numPM4: numPM4 || 0,
							numPM10: numPM10 || 0,
							averageSize: averageSize || 0,
							temperature: temperature || 0,
							type: type || "null",
							windDir: windDir || 0,
							windSpeed: windSpeed || 0,
							humidity: humidity || 0,
							irradiation: irradiation, 
							rainfall: rainfall
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
							PM1Particulates.push(item.massaPM1)
							PM2Particulates.push(item.massaPM2)
							PM4Particulates.push(item.massaPM4)
							PM10Particulates.push(item.massaPM10)
							PM1Numbers.push(item.numPM1)
							PM2Numbers.push(item.numPM2)
							PM4Numbers.push(item.numPM4)
							PM10Numbers.push(item.numPM10)
							averageSizes.push(item.averageSize)
							temperatures.push(item.temperature)
							types.push(item.type)
							windDirections.push(item.windDir)
							windSpeeds.push(item.windSpeed)
							humidities.push(item.humidity)
							irradiations.push(item.irradiation)
							rainfall.push(item.rainfall)
						}
					}
				}

			}

			resolve([
				interval,
				PM1Particulates,
				PM2Particulates,
				PM4Particulates,
				PM10Particulates,
				PM1Numbers,
				PM2Numbers,
				PM4Numbers,
				PM10Numbers,
				averageSizes,
				temperatures,
				types,
				windDirections,
				windSpeeds,
				humidities,
				irradiations, 
				rainfall
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
			'environmental',
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
					PM1Particulates: response[1],
					PM2Particulates: response[2],
					PM4Particulates: response[3],
					PM10Particulates: response[4],
					PM1Numbers: response[5],
					PM2Numbers: response[6],
					PM4Numbers: response[7],
					PM10Numbers: response[8],
					averageSizes: response[9],
					temperatures: response[10],
					windDirections: response[12],
					windSpeeds: response[13],
					humidities: response[14],
					irradiation: response[15],
					rainfall: response[16],
					day: dateToRequest.day,
					month: dateToRequest.month,
					year: dateToRequest.year,
					monthDay: dateToRequest.year + dateToRequest.month + dateToRequest.day,
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
			'irr',
			'massaPM1',
			'massaPM2',
			'massaPM4',
			'massaPM10',
			'numPM1',
			'numPM2',
			'numPM4',
			'numPM10',
			'tamanho_medio',
			'temp',
			'vento_dir',
			'vento_vel',
			'rainfall'
		])

		for (let i = 0; i < response.interval.length; i++) {
			arrayToWrite.push([
				response.monthDay,
				response.interval[i],
				response.irradiation[i], 
				response.PM1Particulates[i],
				response.PM2Particulates[i],
				response.PM4Particulates[i],
				response.PM10Particulates[i],
				response.PM1Numbers[i],
				response.PM2Numbers[i],
				response.PM4Numbers[i],
				response.PM10Numbers[i],
				response.averageSizes[i],
				response.temperatures[i],
				response.windDirections[i],
				response.windSpeeds[i],
				response.rainfall[i]
			])

			console.log("inserindo: " + response.monthDay + " " + response.interval[i] + " irradiacao: " + response.irradiation[i])
		}
		
		csv.write(arrayToWrite, {headers: true}).pipe(ws)

	})
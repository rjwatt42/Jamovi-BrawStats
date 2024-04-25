
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"DVname","title":"Name","type":"String","default":"DV"},{"name":"DVtype","title":"Type","type":"List","options":[{"name":"Interval","title":"Interval"},{"name":"Ordinal","title":"Ordinal"},{"name":"Categorical","title":"Categorical"}],"default":"Interval"},{"name":"DVmu","title":"mean","type":"Number","default":0},{"name":"DVsd","title":"sd","type":"Number","default":1},{"name":"DVskew","title":"skew","type":"Number","default":0},{"name":"DVkurt","title":"kurtosis","type":"Number","default":3},{"name":"DVnlevs","title":"no levels","type":"Number","default":7},{"name":"DViqr","title":"iqr","type":"Number","default":4},{"name":"DVncats","title":"no cases","type":"Number","default":2},{"name":"DVprops","title":"proportions","type":"String","default":"1,1"},{"name":"IVname","title":"Name","type":"String","default":"IV"},{"name":"IVtype","title":"Type","type":"List","options":[{"name":"Interval","title":"Interval"},{"name":"Ordinal","title":"Ordinal"},{"name":"Categorical","title":"Categorical"}],"default":"Interval"},{"name":"IVmu","title":"mean","type":"Number","default":0},{"name":"IVsd","title":"sd","type":"Number","default":1},{"name":"IVskew","title":"skew","type":"Number","default":0},{"name":"IVkurt","title":"kurtosis","type":"Number","default":3},{"name":"IVnlevs","title":"no levels","type":"Number","default":7},{"name":"IViqr","title":"iqr","type":"Number","default":4},{"name":"IVncats","title":"no cases","type":"Number","default":2},{"name":"IVprops","title":"proportions","type":"String","default":"1,1"},{"name":"IV2on","title":" ","type":"Bool"},{"name":"IV2name","title":"Name","type":"String","default":"IV2"},{"name":"IV2type","title":"Type","type":"List","options":[{"name":"Interval","title":"Interval"},{"name":"Ordinal","title":"Ordinal"},{"name":"Categorical","title":"Categorical"}],"default":"Interval"},{"name":"IV2mu","title":"mean","type":"Number","default":0},{"name":"IV2sd","title":"sd","type":"Number","default":1},{"name":"IV2skew","title":"skew","type":"Number","default":0},{"name":"IV2kurt","title":"kurtosis","type":"Number","default":3},{"name":"IV2nlevs","title":"no levels","type":"Number","default":7},{"name":"IV2iqr","title":"iqr","type":"Number","default":4},{"name":"IV2ncats","title":"no cases","type":"Number","default":2},{"name":"IV2props","title":"proportions","type":"String","default":"1,1"},{"name":"EffectSize1","title":"IV->DV","type":"Number","default":0.3},{"name":"EffectSize2","title":"IV2->DV","type":"Number","default":0.3},{"name":"EffectSize3","title":"IV->IV2","type":"Number","default":0.3},{"name":"EffectSize12","title":"IV*IV2->DV","type":"Number","default":0.3},{"name":"WorldOn","title":" ","type":"Bool","default":false},{"name":"WorldPDF","title":"PDF","type":"List","options":[{"name":"Single","title":"Single"},{"name":"Double","title":"Double"},{"name":"Uniform","title":"Uniform"},{"name":"Gauss","title":"Gauss"},{"name":"Exp","title":"Exp"}],"default":"Single"},{"name":"WorldRZ","title":" ","type":"List","options":[{"name":"r","title":"r"},{"name":"z","title":"z"}]},{"name":"Worldk","title":"lambda","type":"Number","default":0.3},{"name":"WorldNullP","title":"p(null)","type":"Number","default":0.5},{"name":"SampleSize","title":"Sample Size","type":"Number","default":42},{"name":"SampleSpread","title":"variable?","type":"List","options":[{"name":"no","title":"no"},{"name":"yes","title":"yes"}],"default":"no"},{"name":"SampleGamma","title":" ","type":"Number","default":1.56},{"name":"SampleMethod","title":"Method","type":"List","options":[{"name":"Random","title":"Random"},{"name":"Stratified","title":"Stratified"},{"name":"Cluster","title":"Cluster"},{"name":"Convenience","title":"Convenience"}],"default":"Random"},{"name":"SampleUsage1","title":"Usage(IV)","type":"List","options":[{"name":"Between","title":"Between"},{"name":"Within","title":"Within"}],"default":"Between"},{"name":"SampleUsage2","title":"Usage(IV2)","type":"List","options":[{"name":"Between","title":"Between"},{"name":"Within","title":"Within"}],"default":"Between"},{"name":"Dependence","title":"Dependence","type":"Number","default":0},{"name":"Outliers","title":"Outliers","type":"Number","default":0},{"name":"Cheating","title":"Method","type":"List","options":[{"name":"None","title":"None"},{"name":"Grow","title":"Grow"},{"name":"Prune","title":"Prune"},{"name":"Replace","title":"Replace"},{"name":"Retry","title":"Retry"}],"default":"None"},{"name":"CheatingAttempts","title":"Attempts","type":"Number","default":5},{"name":"shorthand","title":"shorthand","type":"List","options":[{"name":"no","title":"no"},{"name":"yes","title":"yes"}],"default":"no"},{"name":"Welch","title":"Unequal variance","type":"List","options":[{"name":"no","title":"no"},{"name":"yes","title":"yes"}],"default":"no"},{"name":"Transform","title":"Transform","type":"List","options":[{"name":"None","title":"None"},{"name":"Log","title":"Log"},{"name":"Exp","title":"Exp"}],"default":"None"},{"name":"multipleDoingNull","title":"include nulls","type":"List","options":[{"name":"no","title":"no"},{"name":"yes","title":"yes"}],"default":"no"},{"name":"showHypothesisBtn","title":"show","type":"Action"},{"name":"makeSampleBtn","type":"Action","title":"make","default":false},{"name":"numberSamples","title":"no of samples","type":"Number","default":100},{"name":"makeMultipleBtn","title":"make","type":"Action"},{"name":"showSampleType","title":" show:","type":"List","options":[{"name":"Sample","title":"Sample"},{"name":"Describe","title":"Describe"},{"name":"Infer","title":"Infer"}],"default":"Sample"},{"name":"showInferParam","title":" as:","type":"List","options":[{"name":"Basic","title":"Basic"},{"name":"2D","title":"2D"},{"name":"r","title":"r"},{"name":"p","title":"p"},{"name":"w","title":"w"},{"name":"n","title":"n"},{"name":"rp","title":"rp"},{"name":"wp","title":"wp"},{"name":"nw","title":"nw"}],"default":"Basic"},{"name":"showMultipleParam","title":" show as:","type":"List","options":[{"name":"Basic","title":"Basic"},{"name":"2D","title":"2D"},{"name":"r","title":"r"},{"name":"p","title":"p"},{"name":"w","title":"w"},{"name":"n","title":"n"},{"name":"rp","title":"rp"},{"name":"wp","title":"wp"},{"name":"nw","title":"nw"},{"name":"NHST","title":"NHST"},{"name":"FDR","title":"FDR"},{"name":"FMR","title":"FMR"}],"default":"Basic"},{"name":"whichShowMultiple","title":" ","type":"List","options":[{"name":"direct","title":"direct"},{"name":"unique","title":"unique"},{"name":"total","title":"total"},{"name":"all","title":"all"}],"default":"all"},{"name":"exploreNPoints","title":"exploreNPoints","type":"Number","default":13},{"name":"exploreMaxN","title":"exploreMaxN","type":"Number","default":250},{"name":"exploreDoingNull","title":"include nulls","type":"List","options":[{"name":"no","title":"no"},{"name":"yes","title":"yes"}],"default":"no"},{"name":"exploreNscale","title":"exploreNscale","type":"Bool","default":false},{"name":"numberExplores","title":"no of samples","type":"Number","default":10},{"name":"makeExploreBtn","title":"explore","type":"Action"},{"name":"typeExplore","title":"=","type":"List","options":[{"name":"rIV","title":"EffectSize"},{"name":"rIVIV2","title":"Covariation"},{"name":"n","title":"SampleSize"},{"name":"Method","title":"Method"},{"name":"Usage","title":"Usage"},{"name":"Dependence","title":"Dependence"},{"name":"Outliers","title":"Outliers"},{"name":"Cheating","title":"Cheating"},{"name":"CheatingAmount","title":"CheatAmount"},{"name":"Alpha","title":"Alpha"},{"name":"Transform","title":"Transform"},{"name":"IVType","title":"IVType"},{"name":"DVType","title":"DVType"},{"name":"IVskew","title":"IVskew"},{"name":"IVkurtosis","title":"IVkurtosis"},{"name":"IVprops","title":"IVprops"},{"name":"DVskew","title":"DVskew"},{"name":"DVkurtosis","title":"DVkurtosis"},{"name":"DVprops","title":"DVprops"},{"name":"Heteroscedasticity","title":"Hscedast"}],"default":"n"},{"name":"showExploreParam","title":" show as:","type":"List","options":[{"name":"r","title":"r"},{"name":"p","title":"p"},{"name":"w","title":"w"},{"name":"n","title":"n"},{"name":"rp","title":"rp"},{"name":"wp","title":"wp"},{"name":"wn","title":"wn"},{"name":"p(sig)","title":"p(sig)"},{"name":"NHST","title":"NHST"},{"name":"FDR","title":"FDR"},{"name":"FMR","title":"FMR"}],"default":"r"},{"name":"whichShowExplore","title":" ","type":"List","options":[{"name":"direct","title":"direct"},{"name":"unique","title":"unique"},{"name":"total","title":"total"},{"name":"all","title":"all"}],"default":"all"}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "BrawStats:Simulate Data",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.Label,
			typeName: 'Label',
			label: "Plan",
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					style: "inline",
					controls: [
						{
							type: DefaultControls.CollapseBox,
							typeName: 'CollapseBox',
							collapsed: true,
							label: "Hypothesis",
							margin: "small",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									style: "inline",
									controls: [
										{
											type: DefaultControls.Label,
											typeName: 'Label',
											label: " ",
											verticalAlignment: "center",
											margin: "small",
											minWidth: 13
										},
										{
											type: DefaultControls.LayoutBox,
											typeName: 'LayoutBox',
											style: "list",
											margin: "none",
											controls: [
												{
													type: DefaultControls.CollapseBox,
													typeName: 'CollapseBox',
													collapsed: true,
													label: "Variables",
													margin: "none",
													controls: [
														{
															type: DefaultControls.LayoutBox,
															typeName: 'LayoutBox',
															style: "inline",
															margin: "none",
															controls: [
																{
																	type: DefaultControls.LayoutBox,
																	typeName: 'LayoutBox',
																	style: "inline",
																	margin: "none",
																	controls: [
																		{
																			type: DefaultControls.Label,
																			typeName: 'Label',
																			label: " ",
																			verticalAlignment: "center",
																			margin: "small",
																			minWidth: 13
																		},
																		{
																			type: DefaultControls.CollapseBox,
																			typeName: 'CollapseBox',
																			collapsed: true,
																			label: "IV:",
																			margin: "normal",
																			controls: [
																				{
																					type: DefaultControls.LayoutBox,
																					typeName: 'LayoutBox',
																					style: "inline",
																					margin: "none",
																					controls: [
																						{
																							type: DefaultControls.TextBox,
																							typeName: 'TextBox',
																							name: "IVname",
																							format: FormatDef.string,
																							margin: "normal"
																						},
																						{
																							type: DefaultControls.ComboBox,
																							typeName: 'ComboBox',
																							name: "IVtype",
																							margin: "none"
																						}
																					]
																				},
																				{
																					type: DefaultControls.CollapseBox,
																					typeName: 'CollapseBox',
																					label: "Parameters",
																					collapsed: true,
																					style: "list",
																					margin: "none",
																					controls: [
																						{
																							type: DefaultControls.LayoutBox,
																							typeName: 'LayoutBox',
																							style: "inline",
																							margin: "none",
																							controls: [
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "IVmu",
																									format: FormatDef.number,
																									enable: "(IVtype:Interval)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "IVsd",
																									format: FormatDef.number,
																									enable: "(IVtype:Interval)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "IVskew",
																									format: FormatDef.number,
																									enable: "(IVtype:Interval)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "IVkurt",
																									format: FormatDef.number,
																									enable: "(IVtype:Interval)"
																								}
																							]
																						},
																						{
																							type: DefaultControls.LayoutBox,
																							typeName: 'LayoutBox',
																							style: "inline",
																							margin: "none",
																							controls: [
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "IVnlevs",
																									format: FormatDef.number,
																									enable: "(IVtype:Ordinal)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "IViqr",
																									format: FormatDef.number,
																									enable: "(IVtype:Ordinal)"
																								}
																							]
																						},
																						{
																							type: DefaultControls.LayoutBox,
																							typeName: 'LayoutBox',
																							style: "inline",
																							margin: "none",
																							controls: [
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "IVncats",
																									format: FormatDef.number,
																									enable: "(IVtype:Categorical)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "IVprops",
																									format: FormatDef.string,
																									enable: "(IVtype:Categorical)"
																								}
																							]
																						}
																					]
																				}
																			]
																		}
																	]
																}
															]
														},
														{
															type: DefaultControls.LayoutBox,
															typeName: 'LayoutBox',
															style: "inline",
															margin: "none",
															controls: [
																{
																	type: DefaultControls.Label,
																	typeName: 'Label',
																	label: " ",
																	verticalAlignment: "center",
																	margin: "small",
																	minWidth: 13
																},
																{
																	type: DefaultControls.CollapseBox,
																	typeName: 'CollapseBox',
																	collapsed: true,
																	label: "IV2:",
																	margin: "normal",
																	controls: [
																		{
																			type: DefaultControls.CheckBox,
																			typeName: 'CheckBox',
																			name: "IV2on",
																			margin: "none"
																		},
																		{
																			type: DefaultControls.LayoutBox,
																			typeName: 'LayoutBox',
																			style: "inline",
																			margin: "none",
																			controls: [
																				{
																					type: DefaultControls.TextBox,
																					typeName: 'TextBox',
																					name: "IV2name",
																					format: FormatDef.string,
																					margin: "normal",
																					enable: "(IV2on)"
																				},
																				{
																					type: DefaultControls.ComboBox,
																					typeName: 'ComboBox',
																					name: "IV2type",
																					margin: "none",
																					enable: "(IV2on)"
																				}
																			]
																		},
																		{
																			type: DefaultControls.CollapseBox,
																			typeName: 'CollapseBox',
																			label: "Parameters",
																			collapsed: true,
																			style: "list",
																			margin: "none",
																			controls: [
																				{
																					type: DefaultControls.LayoutBox,
																					typeName: 'LayoutBox',
																					style: "inline",
																					margin: "none",
																					controls: [
																						{
																							type: DefaultControls.TextBox,
																							typeName: 'TextBox',
																							name: "IV2mu",
																							format: FormatDef.number,
																							enable: "(IV2on && IV2type:Interval)"
																						},
																						{
																							type: DefaultControls.TextBox,
																							typeName: 'TextBox',
																							name: "IV2sd",
																							format: FormatDef.number,
																							enable: "(IV2on && IV2type:Interval)"
																						},
																						{
																							type: DefaultControls.TextBox,
																							typeName: 'TextBox',
																							name: "IV2skew",
																							format: FormatDef.number,
																							enable: "(IV2on && IV2type:Interval)"
																						},
																						{
																							type: DefaultControls.TextBox,
																							typeName: 'TextBox',
																							name: "IV2kurt",
																							format: FormatDef.number,
																							enable: "(IV2on && IV2type:Interval)"
																						}
																					]
																				},
																				{
																					type: DefaultControls.LayoutBox,
																					typeName: 'LayoutBox',
																					style: "inline",
																					margin: "none",
																					controls: [
																						{
																							type: DefaultControls.TextBox,
																							typeName: 'TextBox',
																							name: "IV2nlevs",
																							format: FormatDef.number,
																							enable: "(IV2on && IV2type:Ordinal)"
																						},
																						{
																							type: DefaultControls.TextBox,
																							typeName: 'TextBox',
																							name: "IV2iqr",
																							format: FormatDef.number,
																							enable: "(IV2on && IV2type:Ordinal)"
																						}
																					]
																				},
																				{
																					type: DefaultControls.LayoutBox,
																					typeName: 'LayoutBox',
																					style: "inline",
																					margin: "none",
																					controls: [
																						{
																							type: DefaultControls.TextBox,
																							typeName: 'TextBox',
																							name: "IV2ncats",
																							format: FormatDef.number,
																							enable: "(IV2on && IV2type:Categorical)"
																						},
																						{
																							type: DefaultControls.TextBox,
																							typeName: 'TextBox',
																							name: "IV2props",
																							format: FormatDef.string,
																							enable: "(IV2on && IV2type:Categorical)"
																						}
																					]
																				}
																			]
																		}
																	]
																}
															]
														},
														{
															type: DefaultControls.LayoutBox,
															typeName: 'LayoutBox',
															style: "inline",
															margin: "none",
															controls: [
																{
																	type: DefaultControls.LayoutBox,
																	typeName: 'LayoutBox',
																	style: "inline",
																	margin: "none",
																	controls: [
																		{
																			type: DefaultControls.Label,
																			typeName: 'Label',
																			label: " ",
																			verticalAlignment: "center",
																			margin: "small",
																			minWidth: 13
																		},
																		{
																			type: DefaultControls.CollapseBox,
																			typeName: 'CollapseBox',
																			collapsed: true,
																			label: "DV:",
																			margin: "normal",
																			controls: [
																				{
																					type: DefaultControls.LayoutBox,
																					typeName: 'LayoutBox',
																					style: "inline",
																					margin: "none",
																					controls: [
																						{
																							type: DefaultControls.LayoutBox,
																							typeName: 'LayoutBox',
																							style: "inline",
																							margin: "none",
																							controls: [
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "DVname",
																									format: FormatDef.string,
																									margin: "normal"
																								},
																								{
																									type: DefaultControls.ComboBox,
																									typeName: 'ComboBox',
																									name: "DVtype",
																									margin: "none"
																								}
																							]
																						}
																					]
																				},
																				{
																					type: DefaultControls.CollapseBox,
																					typeName: 'CollapseBox',
																					label: "Parameters",
																					collapsed: true,
																					style: "list",
																					margin: "none",
																					controls: [
																						{
																							type: DefaultControls.LayoutBox,
																							typeName: 'LayoutBox',
																							style: "inline",
																							margin: "none",
																							controls: [
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "DVmu",
																									format: FormatDef.number,
																									enable: "(DVtype:Interval)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "DVsd",
																									format: FormatDef.number,
																									enable: "(DVtype:Interval)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "DVskew",
																									format: FormatDef.number,
																									enable: "(DVtype:Interval)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "DVkurt",
																									format: FormatDef.number,
																									enable: "(DVtype:Interval)"
																								}
																							]
																						},
																						{
																							type: DefaultControls.LayoutBox,
																							typeName: 'LayoutBox',
																							style: "inline",
																							margin: "none",
																							controls: [
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "DVnlevs",
																									format: FormatDef.number,
																									enable: "(DVtype:Ordinal)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "DViqr",
																									format: FormatDef.number,
																									enable: "(DVtype:Ordinal)"
																								}
																							]
																						},
																						{
																							type: DefaultControls.LayoutBox,
																							typeName: 'LayoutBox',
																							style: "inline",
																							margin: "none",
																							controls: [
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "DVncats",
																									format: FormatDef.number,
																									enable: "(DVtype:Categorical)"
																								},
																								{
																									type: DefaultControls.TextBox,
																									typeName: 'TextBox',
																									name: "DVprops",
																									format: FormatDef.string,
																									enable: "(DVtype:Categorical)"
																								}
																							]
																						}
																					]
																				}
																			]
																		}
																	]
																}
															]
														}
													]
												},
												{
													type: DefaultControls.CollapseBox,
													typeName: 'CollapseBox',
													collapsed: true,
													label: "Effects",
													margin: "none",
													controls: [
														{
															type: DefaultControls.LayoutBox,
															typeName: 'LayoutBox',
															margin: "none",
															controls: [
																{
																	type: DefaultControls.TextBox,
																	typeName: 'TextBox',
																	name: "EffectSize1",
																	margin: "none",
																	format: FormatDef.number
																},
																{
																	type: DefaultControls.TextBox,
																	typeName: 'TextBox',
																	name: "EffectSize2",
																	margin: "none",
																	format: FormatDef.number,
																	enable: "(IV2on)"
																},
																{
																	type: DefaultControls.TextBox,
																	typeName: 'TextBox',
																	name: "EffectSize3",
																	margin: "none",
																	format: FormatDef.number,
																	enable: "(IV2on)"
																},
																{
																	type: DefaultControls.TextBox,
																	typeName: 'TextBox',
																	name: "EffectSize12",
																	margin: "none",
																	format: FormatDef.number,
																	enable: "(IV2on)"
																}
															]
														},
														{
															type: DefaultControls.Label,
															typeName: 'Label',
															label: " ",
															margin: "small"
														}
													]
												},
												{
													type: DefaultControls.CollapseBox,
													typeName: 'CollapseBox',
													collapsed: true,
													label: "World",
													margin: "none",
													controls: [
														{
															type: DefaultControls.LayoutBox,
															typeName: 'LayoutBox',
															style: "inline",
															margin: "none",
															controls: [
																{
																	type: DefaultControls.CheckBox,
																	typeName: 'CheckBox',
																	name: "WorldOn"
																},
																{
																	type: DefaultControls.ComboBox,
																	typeName: 'ComboBox',
																	name: "WorldPDF"
																},
																{
																	type: DefaultControls.ComboBox,
																	typeName: 'ComboBox',
																	name: "WorldRZ"
																},
																{
																	type: DefaultControls.TextBox,
																	typeName: 'TextBox',
																	name: "Worldk",
																	format: FormatDef.number
																},
																{
																	type: DefaultControls.TextBox,
																	typeName: 'TextBox',
																	name: "WorldNullP",
																	format: FormatDef.number
																}
															]
														}
													]
												}
											]
										}
									]
								}
							]
						},
						{
							type: DefaultControls.ActionButton,
							typeName: 'ActionButton',
							name: "showHypothesisBtn"
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					style: "inline",
					controls: [
						{
							type: DefaultControls.CollapseBox,
							typeName: 'CollapseBox',
							collapsed: true,
							label: "Design",
							margin: "none",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									style: "inline",
									controls: [
										{
											type: DefaultControls.Label,
											typeName: 'Label',
											label: " ",
											verticalAlignment: "center",
											margin: "small",
											minWidth: 13
										},
										{
											type: DefaultControls.LayoutBox,
											typeName: 'LayoutBox',
											style: "list",
											margin: "none",
											controls: [
												{
													type: DefaultControls.CollapseBox,
													typeName: 'CollapseBox',
													label: "Sampling",
													collapsed: true,
													margin: "none",
													controls: [
														{
															type: DefaultControls.LayoutBox,
															typeName: 'LayoutBox',
															style: "list",
															margin: "none",
															controls: [
																{
																	type: DefaultControls.LayoutBox,
																	typeName: 'LayoutBox',
																	style: "inline",
																	margin: "none",
																	controls: [
																		{
																			type: DefaultControls.TextBox,
																			typeName: 'TextBox',
																			name: "SampleSize",
																			margin: "none",
																			format: FormatDef.number
																		},
																		{
																			type: DefaultControls.ComboBox,
																			typeName: 'ComboBox',
																			name: "SampleSpread",
																			margin: "none"
																		},
																		{
																			type: DefaultControls.TextBox,
																			typeName: 'TextBox',
																			name: "SampleGamma",
																			margin: "none",
																			format: FormatDef.number
																		}
																	]
																},
																{
																	type: DefaultControls.LayoutBox,
																	typeName: 'LayoutBox',
																	style: "inline",
																	margin: "none",
																	controls: [
																		{
																			type: DefaultControls.ComboBox,
																			typeName: 'ComboBox',
																			name: "SampleMethod",
																			margin: "none"
																		}
																	]
																},
																{
																	type: DefaultControls.LayoutBox,
																	typeName: 'LayoutBox',
																	style: "inline",
																	margin: "none",
																	controls: [
																		{
																			type: DefaultControls.ComboBox,
																			typeName: 'ComboBox',
																			name: "SampleUsage1",
																			margin: "none"
																		},
																		{
																			type: DefaultControls.ComboBox,
																			typeName: 'ComboBox',
																			name: "SampleUsage2",
																			margin: "none"
																		}
																	]
																}
															]
														}
													]
												},
												{
													type: DefaultControls.CollapseBox,
													typeName: 'CollapseBox',
													label: "Anomalies",
													collapsed: true,
													margin: "none",
													controls: [
														{
															type: DefaultControls.LayoutBox,
															typeName: 'LayoutBox',
															style: "list",
															margin: "none",
															controls: [
																{
																	type: DefaultControls.LayoutBox,
																	typeName: 'LayoutBox',
																	style: "inline",
																	margin: "none",
																	controls: [
																		{
																			type: DefaultControls.TextBox,
																			typeName: 'TextBox',
																			name: "Dependence",
																			margin: "none",
																			format: FormatDef.number
																		},
																		{
																			type: DefaultControls.TextBox,
																			typeName: 'TextBox',
																			name: "Outliers",
																			margin: "none",
																			format: FormatDef.number
																		}
																	]
																}
															]
														}
													]
												},
												{
													type: DefaultControls.CollapseBox,
													typeName: 'CollapseBox',
													label: "Cheating",
													collapsed: true,
													margin: "none",
													controls: [
														{
															type: DefaultControls.LayoutBox,
															typeName: 'LayoutBox',
															style: "list",
															margin: "none",
															controls: [
																{
																	type: DefaultControls.LayoutBox,
																	typeName: 'LayoutBox',
																	style: "inline",
																	margin: "none",
																	controls: [
																		{
																			type: DefaultControls.ComboBox,
																			typeName: 'ComboBox',
																			name: "Cheating",
																			margin: "none"
																		},
																		{
																			type: DefaultControls.TextBox,
																			typeName: 'TextBox',
																			name: "CheatingAttempts",
																			margin: "none",
																			format: FormatDef.number
																		}
																	]
																}
															]
														}
													]
												}
											]
										}
									]
								}
							]
						}
					]
				},
				{
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					collapsed: true,
					label: "Analysis",
					margin: "none",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							style: "inline",
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: " ",
									verticalAlignment: "center",
									margin: "small",
									minWidth: 13
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									style: "inline",
									margin: "none",
									controls: [
										{
											type: DefaultControls.ComboBox,
											typeName: 'ComboBox',
											name: "Welch"
										},
										{
											type: DefaultControls.ComboBox,
											typeName: 'ComboBox',
											name: "Transform"
										},
										{
											type: DefaultControls.ComboBox,
											typeName: 'ComboBox',
											name: "shorthand"
										}
									]
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.Label,
			typeName: 'Label',
			label: "Sample",
			controls: [
				{
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					label: "Single Sample",
					collapsed: true,
					margin: "none",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							style: "inline",
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: " ",
									margin: "small",
									minWidth: 13
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					style: "inline",
					margin: "none",
					controls: [
						{
							type: DefaultControls.ActionButton,
							typeName: 'ActionButton',
							name: "makeSampleBtn"
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "showSampleType"
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "showInferParam"
						}
					]
				},
				{
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					label: "Multiple Samples",
					collapsed: true,
					margin: "none",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							style: "inline",
							margin: "none",
							controls: [
								{
									name: "numberSamples",
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									format: FormatDef.number
								},
								{
									name: "multipleDoingNull",
									type: DefaultControls.ComboBox,
									typeName: 'ComboBox'
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					style: "inline",
					margin: "none",
					controls: [
						{
							name: "makeMultipleBtn",
							type: DefaultControls.ActionButton,
							typeName: 'ActionButton'
						},
						{
							name: "showMultipleParam",
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox'
						},
						{
							name: "whichShowMultiple",
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox'
						}
					]
				}
			]
		},
		{
			type: DefaultControls.Label,
			typeName: 'Label',
			label: "Explore",
			controls: [
				{
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					label: "Explore Parameters",
					collapsed: true,
					margin: "none",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							style: "inline",
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: " ",
									margin: "small",
									minWidth: 13
								},
								{
									type: DefaultControls.CollapseBox,
									typeName: 'CollapseBox',
									label: "Options",
									collapsed: true,
									margin: "none",
									controls: [
										{
											type: DefaultControls.LayoutBox,
											typeName: 'LayoutBox',
											style: "inline",
											controls: [
												{
													type: DefaultControls.TextBox,
													typeName: 'TextBox',
													name: "exploreNPoints",
													format: FormatDef.number
												}
											]
										},
										{
											type: DefaultControls.LayoutBox,
											typeName: 'LayoutBox',
											style: "inline",
											controls: [
												{
													type: DefaultControls.TextBox,
													typeName: 'TextBox',
													name: "exploreMaxN",
													format: FormatDef.number
												},
												{
													type: DefaultControls.CheckBox,
													typeName: 'CheckBox',
													name: "exploreNscale"
												}
											]
										}
									]
								}
							]
						},
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							style: "inline",
							margin: "none",
							controls: [
								{
									name: "numberExplores",
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									format: FormatDef.number
								},
								{
									name: "exploreDoingNull",
									type: DefaultControls.ComboBox,
									typeName: 'ComboBox'
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					style: "inline",
					margin: "none",
					controls: [
						{
							name: "makeExploreBtn",
							type: DefaultControls.ActionButton,
							typeName: 'ActionButton'
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "typeExplore"
						},
						{
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox',
							name: "showExploreParam"
						},
						{
							name: "whichShowExplore",
							type: DefaultControls.ComboBox,
							typeName: 'ComboBox'
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };

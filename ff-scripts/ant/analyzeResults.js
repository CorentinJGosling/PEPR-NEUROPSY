//This is where all the heavy lifting happens.  This is the statistical analysis of the results.

//userInfo: 0-userID, 1-age, 2-gender, 3-sessionNumber, 4-studyID, 5-groupID, 6-stimulusType, 7-sessionStartDate, 8-sessionEndDate, 9-displaySize, 10-displayWidth, 11-displayHeight, 12-ppi

//Note that results contains 0-practice, 1-test1, 2-test2;
//Results: 0-cueType, 1-targetType, 2-targetPosition, 3-cuePosition, 4-targetDirection, 5-flankDirection, 6-stage1Time, 7-stage4Time, 8-keyPressed, 9-correct, 10-trialStartDate, 11-targetOnTime

function generateSummary(userInfo, results) {
	//These are sub-arrays, filtered versions of the full array
	var allCorrectRT = [];
	var C1T1 = [];
	var C1T2 = [];
	var C2T1 = [];
	var C2T2 = [];
	var C3T1 = [];
	var C3T2 = [];
	var C4T1 = [];
	var C4T2 = [];
	var allResults = [];

	// Join all results sets
	for (i in results) {
		// discard the practice round
		if (i >= 1) {
			allResults = allResults.concat(results[i]);
		}
	}

	// ===== STEP 1: CLASSIFICATION HIÉRARCHIQUE DES ERREURS (conforme au script R) =====
	// Selon le script R (lignes 104-129), la hiérarchie est:
	// 1. is_omission (RT = 0 ou NA)
	// 2. is_perseveration (RT < 200ms)
	// 3. is_too_slow (RT > 1700ms)
	// 4. is_valid_timing (200 <= RT <= 1700)
	// 5. is_outlier (calculé après sur valid_timing)
	// 6. is_wrong_response (Correct = 0 & valid_timing & !outlier)
	// 7. is_hit (Correct = 1 & valid_timing & !outlier)
	
	for (i in allResults) {
		var rt = allResults[i][7];
		var keyPressed = allResults[i][8];
		var targetDirection = allResults[i][4];
		
		// Classification hiérarchique
		// Conforme au script R (lignes 79-80, 105-108):
		// RT = ifelse(RT == 0 | Response == "None", NA, as.numeric(RT))
		// is_omission = (is.na(RT) | RT == 0)
		// Dans le JS, quand timeout: RT=1700 et keyPressed=0 (voir trial.js ligne 80-81)
		// is_valid_timing = (!is.na(RT) & RT >= RT_MIN & RT <= RT_MAX)
		// Note: RT=1700 est considéré comme valid_timing dans le R (1700 <= 1700 = TRUE)
		var isRTMissing = (rt === null || rt === undefined || isNaN(rt) || rt === 0 || 
		                   keyPressed === 0 || keyPressed === null || keyPressed === undefined);
		allResults[i].is_omission = isRTMissing;
		allResults[i].is_perseveration = (!isRTMissing && rt > 0 && rt < 200);
		allResults[i].is_too_slow = (!isRTMissing && rt > 0 && rt > 1700);
		allResults[i].is_valid_timing = (!isRTMissing && rt >= 200 && rt <= 1700);
		
		// Correct = bonne touche pressée (indépendamment du timing)
		// Conforme au script R qui lit Correct du CSV sans modification
		var correctKey = ((targetDirection == 'L' && keyPressed == 70) || (targetDirection == 'R' && keyPressed == 74));
		allResults[i][9] = correctKey ? 1 : 0;
		
		// Collecter les RT corrects (avant exclusion outliers)
		if (allResults[i][9] === 1) {
			allCorrectRT.push(rt);
		}
		
		// Trier par condition (8 conditions)
		if ((allResults[i][0] == 1) && (allResults[i][1] == 'congruent')) {
			C1T1.push(allResults[i]);
		} else if ((allResults[i][0] == 1) && (allResults[i][1] == 'incongruent')) {
			C1T2.push(allResults[i]);
		} else if ((allResults[i][0] == 2) && (allResults[i][1] == 'congruent')) {
			C2T1.push(allResults[i]);
		} else if ((allResults[i][0] == 2) && (allResults[i][1] == 'incongruent')) {
			C2T2.push(allResults[i]);
		} else if ((allResults[i][0] == 3) && (allResults[i][1] == 'congruent')) {
			C3T1.push(allResults[i]);
		} else if ((allResults[i][0] == 3) && (allResults[i][1] == 'incongruent')) {
			C3T2.push(allResults[i]);
		} else if ((allResults[i][0] == 4) && (allResults[i][1] == 'congruent')) {
			C4T1.push(allResults[i]);
		} else if ((allResults[i][0] == 4) && (allResults[i][1] == 'incongruent')) {
			C4T2.push(allResults[i]);
		}
	}

	var summary = [];
	// The first row is the headers
	summary[0] = ['uniqueID', 'studyID', 'ANTversion', 'targFile', 'ANTdate', 'ANTtime', 'SessionDur', 'Session', 'Age', 'Sex', 'Group', 'ANT.N', 'med.all', 'mean.all', 'sd.all', 'min.all', 'max.all', 'alert', 'orient', 'conflict', 'pc.all', 'e.all', 'nocue', 'double', 'centre', 'spatial', 'cong', 'incong', 'med.C1T1', 'med.C1T2', 'med.C2T1', 'med.C2T2', 'med.C3T1', 'med.C3T2', 'med.C4T1', 'med.C4T2', 'mean.C1T1', 'mean.C1T2', 'mean.C2T1', 'mean.C2T2', 'mean.C3T1', 'mean.C3T2', 'mean.C4T1', 'mean.C4T2', 'e.nocue', 'e.double', 'e.centre', 'e.spatial', 'e.incong', 'e.cong', 'pc.C1T1', 'pc.C1T2', 'pc.C2T1', 'pc.C2T2', 'pc.C3T1', 'pc.C3T2', 'pc.C4T1', 'pc.C4T2', 'mean.RT.correct', 'sd.RT.correct', 'omitted_responses_count', 'impulsive_responses_count', 'outliers_3sd', 'outliers_2sd', 'hits_rt_se_variability', 'hits_rt_se', 'wrong_responses_count', 'hits_rt_block_change', 'hits_se_block_change'];
	summary[1] = [];
	summary[1][0] = userInfo[0];							//userID
	summary[1][1] = userInfo[4];							//studyID
	summary[1][2] = softwareVersion;						//ANTversion
	summary[1][3] = userInfo[6];							//targFile
	summary[1][4] = formatDate(userInfo[7]);				//ANTdate
	summary[1][5] = formatTime(userInfo[7]);				//ANTtime
	summary[1][6] = formatMilliseconds(userInfo[8] - userInfo[7]);	//SessionDur
	summary[1][7] = userInfo[3];							//Session
	summary[1][8] = userInfo[1];							//Age
	summary[1][9] = userInfo[2];							//Sex
	summary[1][10] = userInfo[5];							//Group
	// ANT.N sera calculé après la détection des outliers (= nombre de hits)
	
	// ===== STEP 2: COMPTAGE DES ERREURS (avant calcul des outliers) =====
	// 60. omitted_responses_count
	summary[1][60] = allResults.filter(trial => trial.is_omission).length;
	
	// 61. impulsive_responses_count (persévérations)
	summary[1][61] = allResults.filter(trial => trial.is_perseveration).length;

	// ===== STEP 3: DÉTECTION DES OUTLIERS 3SD PAR CONDITION =====
	// Selon le script R (lignes 89-116): outliers calculés sur essais corrects avec timing valide
	
	// Initialiser is_outlier_3sd et is_outlier_2sd à false pour tous les essais
	for (i in allResults) {
		allResults[i].is_outlier_3sd = false;
		allResults[i].is_outlier_2sd = false;
	}
	
	var conditions = [
		{ name: 'C1T1', data: C1T1 },
		{ name: 'C1T2', data: C1T2 },
		{ name: 'C2T1', data: C2T1 },
		{ name: 'C2T2', data: C2T2 },
		{ name: 'C3T1', data: C3T1 },
		{ name: 'C3T2', data: C3T2 },
		{ name: 'C4T1', data: C4T1 },
		{ name: 'C4T2', data: C4T2 }
	];
	
	// Calculer mean/SD par condition et marquer les outliers
	var outliers3SD = 0;
	var outliers2SD = 0;
	
	for (var c = 0; c < conditions.length; c++) {
		// Filtrer: correct + valid_timing
		var conditionCorrectRT = conditions[c].data
			.filter(trial => trial[9] === 1 && trial.is_valid_timing)
			.map(trial => trial[7]);
		
		if (conditionCorrectRT.length > 0) {
			var condMean = conditionCorrectRT.reduce((a, b) => a + b, 0) / conditionCorrectRT.length;
			var condSD = standardDeviation(conditionCorrectRT);
			
			// Marquer les outliers dans les trials originaux
			for (var trial of conditions[c].data) {
				if (trial[9] === 1 && trial.is_valid_timing) {
					var rt = trial[7];
					// Outlier 3SD
					if (rt >= condMean + 3 * condSD || rt <= condMean - 3 * condSD) {
						trial.is_outlier_3sd = true;
						outliers3SD++;
					}
					// Outlier 2SD
					if (rt >= condMean + 2 * condSD || rt <= condMean - 2 * condSD) {
						trial.is_outlier_2sd = true;
						outliers2SD++;
					}
				}
			}
		}
	}
	
	// Marquer is_hit et is_wrong_response (après détection outliers)
	for (i in allResults) {
		allResults[i].is_hit = (allResults[i][9] === 1 && allResults[i].is_valid_timing && !allResults[i].is_outlier_3sd);
		allResults[i].is_wrong_response = (allResults[i][9] === 0 && allResults[i].is_valid_timing && !allResults[i].is_outlier_3sd);
	}
	
	// ANT.N = nombre de hits (conforme au script R ligne 387: ANT.N = nrow(subj_hits))
	summary[1][11] = allResults.filter(trial => trial.is_hit).length;
	
	summary[1][62] = outliers3SD;
	summary[1][63] = outliers2SD;

	// ===== STEP 4: RECALCULER LES STATISTIQUES GLOBALES (hits uniquement, sans outliers) =====
	// Selon le script R (ligne 154-163): stats calculées sur is_hit (correct + valid_timing + !outlier)
	var allHitsRT = allResults.filter(trial => trial.is_hit).map(trial => trial[7]);
	
	// Statistiques globales (sur hits uniquement)
	summary[1][12] = allHitsRT.length > 0 ? Math.round(median(allHitsRT) * 100) / 100 : 0;  // med.all
	summary[1][13] = allHitsRT.length > 0 ? Math.round(mean(allHitsRT) * 100) / 100 : 0;    // mean.all
	summary[1][14] = allHitsRT.length > 0 ? Math.round(standardDeviation(allHitsRT) * 100) / 100 : 0;  // sd.all
	summary[1][15] = allHitsRT.length > 0 ? Math.round(Math.min(...allHitsRT) * 100) / 100 : 0;  // min.all
	summary[1][16] = allHitsRT.length > 0 ? Math.round(Math.max(...allHitsRT) * 100) / 100 : 0;  // max.all
	
	// 58. mean.RT.correct (identique à mean.all car calculé sur hits)
	summary[1][58] = summary[1][13];
	
	// 59. sd.RT.correct (identique à sd.all car calculé sur hits)
	summary[1][59] = summary[1][14];

	// 64. hits_rt_se_variability - SD des SE par bloc
	var blocks = {};
	for (var i = 0; i < results.length; i++) {
		if (i >= 1) { // Exclure le bloc d'entraînement
			var blockHitsRT = [];
			for (var j = 0; j < results[i].length; j++) {
				// Utiliser is_hit au lieu de isCorrect
				if (results[i][j].is_hit) {
					blockHitsRT.push(results[i][j][7]);
				}
			}
			if (blockHitsRT.length > 0) {
				var blockSD = standardDeviation(blockHitsRT);
				blocks[i] = blockSD / Math.sqrt(blockHitsRT.length);
			}
		}
	}
	
	var blockSEArray = [];
	for (var blockNum in blocks) {
		blockSEArray.push(blocks[blockNum]);
	}
	summary[1][64] = blockSEArray.length > 0 ? Math.round(standardDeviation(blockSEArray) * 100) / 100 : 0;
	
	// 65. hits_rt_se - Erreur standard globale (sur hits uniquement)
	if (allHitsRT.length > 0) {
		var rtSD = standardDeviation(allHitsRT);
		summary[1][65] = Math.round((rtSD / Math.sqrt(allHitsRT.length)) * 100) / 100;
	} else {
		summary[1][65] = 0;
	}

	// 66. wrong_responses_count - Selon le script R (ligne 351): is_wrong_response = Correct == 0 & valid_timing & !outlier
	summary[1][66] = allResults.filter(trial => trial.is_wrong_response).length;

	// 67. hits_rt_block_change - Pente du changement de RT moyen entre blocs (sur hits)
	var blockMeans = [];
	for (let i = 0; i < results.length; i++) {
		if (i >= 1) {
			let blockHitsRT = [];
			for (let j = 0; j < results[i].length; j++) {
				if (results[i][j].is_hit) {
					blockHitsRT.push(results[i][j][7]);
				}
			}
			if (blockHitsRT.length > 0) {
				let blockMean = blockHitsRT.reduce((a, b) => a + b, 0) / blockHitsRT.length;
				blockMeans.push(blockMean);
			}
		}
	}
	summary[1][67] = blockMeans.length >= 2 ? Math.round(calculateSlope(blockMeans) * 100) / 100 : 0;
	
	// 68. hits_se_block_change - Pente du changement de SE entre blocs
	summary[1][68] = blockSEArray.length >= 2 ? Math.round(calculateSlope(blockSEArray) * 100) / 100 : 0;

	//Now we're going to work from the bottom up, because it makes the math easier
	// pc = Percent Correct (accuracy), calculated on valid timing trials (200-1700ms, non-outliers)
	// Selon le script R: accuracy = n_hits / n_valid_trials

	// Helper function pour calculer l'accuracy : n_hits / n_valid
	// Selon le script R (ligne 148): subj_valid = filter(is_valid_timing, !is_outlier)
	// Puis lignes 287-312: accuracy = (n_hits / n_valid) * 100
	// n_valid = essais avec is_valid_timing ET !is_outlier
	// n_hits = essais avec is_hit (correct + valid_timing + !outlier)
	function calculateAccuracy(conditionData) {
		var validTrials = conditionData.filter(trial => trial.is_valid_timing && !trial.is_outlier_3sd);
		if (validTrials.length === 0) return 0;
		var hitTrials = conditionData.filter(trial => trial.is_hit);
		return Math.round((hitTrials.length / validTrials.length) * 10000) / 100; // pourcentage avec 2 décimales
	}

	totalNoCue = C1T1.concat(C1T2);
	totalDouble = C3T1.concat(C3T2);
	totalCentre = C2T1.concat(C2T2);
	totalSpatial = C4T1.concat(C4T2);
	totalCong = C1T1.concat(C2T1, C3T1, C4T1);
	totalIncong = C1T2.concat(C2T2, C3T2, C4T2);
	
	summary[1][57] = calculateAccuracy(C4T2);				//pc.C4T2
	summary[1][56] = calculateAccuracy(C4T1);				//pc.C4T1
	summary[1][55] = calculateAccuracy(C3T2);				//pc.C3T2
	summary[1][54] = calculateAccuracy(C3T1);				//pc.C3T1
	summary[1][53] = calculateAccuracy(C2T2);				//pc.C2T2
	summary[1][52] = calculateAccuracy(C2T1);				//pc.C2T1
	summary[1][51] = calculateAccuracy(C1T2);				//pc.C1T2
	summary[1][50] = calculateAccuracy(C1T1);				//pc.C1T1

	// Taux d'erreur par type (calculés sur essais avec timing valide, AVEC exclusion outliers)
	// Selon le script R (ligne 148): subj_valid = filter(is_valid_timing, !is_outlier)
	// Puis lignes 244-246: error_rate = (1 - safe_mean(Correct)) * 100 sur subj_valid
	function calculateErrorRate(conditionData) {
		var validTrials = conditionData.filter(trial => trial.is_valid_timing && !trial.is_outlier_3sd);
		if (validTrials.length === 0) return 0;
		// Compter les essais corrects (Correct == 1) parmi les valid_timing sans outliers
		var correctTrials = validTrials.filter(trial => trial[9] === 1);
		return Math.round((1 - (correctTrials.length / validTrials.length)) * 10000) / 100;
	}

	summary[1][48] = calculateErrorRate(totalIncong);		//e.incong
	summary[1][49] = calculateErrorRate(totalCong);			//e.cong
	totalSpatial = C4T1.concat(C4T2);
	summary[1][47] = calculateErrorRate(totalSpatial);		//e.spatial
	totalCentre = C2T1.concat(C2T2);
	summary[1][46] = calculateErrorRate(totalCentre);		//e.centre
	totalDouble = C3T1.concat(C3T2);
	summary[1][45] = calculateErrorRate(totalDouble);		//e.double
	totalNoCue = C1T1.concat(C1T2);
	summary[1][44] = calculateErrorRate(totalNoCue);		//e.nocue

	// Get the response time values for hits (correct + valid_timing + !outlier)
	var C1T1RT = C1T1.filter(trial => trial.is_hit).map(trial => trial[7]);
	var C1T2RT = C1T2.filter(trial => trial.is_hit).map(trial => trial[7]);
	var C2T1RT = C2T1.filter(trial => trial.is_hit).map(trial => trial[7]);
	var C2T2RT = C2T2.filter(trial => trial.is_hit).map(trial => trial[7]);
	var C3T1RT = C3T1.filter(trial => trial.is_hit).map(trial => trial[7]);
	var C3T2RT = C3T2.filter(trial => trial.is_hit).map(trial => trial[7]);
	var C4T1RT = C4T1.filter(trial => trial.is_hit).map(trial => trial[7]);
	var C4T2RT = C4T2.filter(trial => trial.is_hit).map(trial => trial[7]);

	var totalNoCueRT = totalNoCue.filter(trial => trial.is_hit).map(trial => trial[7]);
	var totalDoubleRT = totalDouble.filter(trial => trial.is_hit).map(trial => trial[7]);
	var totalCentreRT = totalCentre.filter(trial => trial.is_hit).map(trial => trial[7]);
	var totalSpatialRT = totalSpatial.filter(trial => trial.is_hit).map(trial => trial[7]);
	var totalCongRT = totalCong.filter(trial => trial.is_hit).map(trial => trial[7]);
	var totalIncongRT = totalIncong.filter(trial => trial.is_hit).map(trial => trial[7]);

	// Moyennes par condition (arrondies à 2 décimales)
	summary[1][43] = C4T2RT.length > 0 ? Math.round(mean(C4T2RT) * 100) / 100 : 0;  // mean.C4T2
	summary[1][42] = C4T1RT.length > 0 ? Math.round(mean(C4T1RT) * 100) / 100 : 0;  // mean.C4T1
	summary[1][41] = C3T2RT.length > 0 ? Math.round(mean(C3T2RT) * 100) / 100 : 0;  // mean.C3T2
	summary[1][40] = C3T1RT.length > 0 ? Math.round(mean(C3T1RT) * 100) / 100 : 0;  // mean.C3T1
	summary[1][39] = C2T2RT.length > 0 ? Math.round(mean(C2T2RT) * 100) / 100 : 0;  // mean.C2T2
	summary[1][38] = C2T1RT.length > 0 ? Math.round(mean(C2T1RT) * 100) / 100 : 0;  // mean.C2T1
	summary[1][37] = C1T2RT.length > 0 ? Math.round(mean(C1T2RT) * 100) / 100 : 0;  // mean.C1T2
	summary[1][36] = C1T1RT.length > 0 ? Math.round(mean(C1T1RT) * 100) / 100 : 0;  // mean.C1T1
	
	// Médianes par condition (arrondies à 2 décimales)
	summary[1][35] = C4T2RT.length > 0 ? Math.round(median(C4T2RT) * 100) / 100 : 0;  // med.C4T2
	summary[1][34] = C4T1RT.length > 0 ? Math.round(median(C4T1RT) * 100) / 100 : 0;  // med.C4T1
	summary[1][33] = C3T2RT.length > 0 ? Math.round(median(C3T2RT) * 100) / 100 : 0;  // med.C3T2
	summary[1][32] = C3T1RT.length > 0 ? Math.round(median(C3T1RT) * 100) / 100 : 0;  // med.C3T1
	summary[1][31] = C2T2RT.length > 0 ? Math.round(median(C2T2RT) * 100) / 100 : 0;  // med.C2T2
	summary[1][30] = C2T1RT.length > 0 ? Math.round(median(C2T1RT) * 100) / 100 : 0;  // med.C2T1
	summary[1][29] = C1T2RT.length > 0 ? Math.round(median(C1T2RT) * 100) / 100 : 0;  // med.C1T2
	summary[1][28] = C1T1RT.length > 0 ? Math.round(median(C1T1RT) * 100) / 100 : 0;  // med.C1T1

	// Indices réseau: moyennes des RT par type (conforme au script R lignes 175-190)
	// Le script R calcule la moyenne de TOUS les RT hits du même type, pas la moyenne des moyennes
	
	// Regrouper tous les hits par type de flanker (congruent vs incongruent)
	var allIncongHits = C1T2.concat(C2T2, C3T2, C4T2).filter(trial => trial.is_hit).map(trial => trial[7]);
	var allCongHits = C1T1.concat(C2T1, C3T1, C4T1).filter(trial => trial.is_hit).map(trial => trial[7]);
	
	summary[1][27] = allIncongHits.length > 0 ? Math.round((allIncongHits.reduce((a,b) => a+b, 0) / allIncongHits.length) * 100) / 100 : 0;  // incong
	summary[1][26] = allCongHits.length > 0 ? Math.round((allCongHits.reduce((a,b) => a+b, 0) / allCongHits.length) * 100) / 100 : 0;  // cong
	
	// Regrouper tous les hits par type de cue
	// CueType 1 = no cue, 2 = center, 3 = double, 4 = spatial
	var allSpatialHits = C4T1.concat(C4T2).filter(trial => trial.is_hit).map(trial => trial[7]);
	var allCentreHits = C2T1.concat(C2T2).filter(trial => trial.is_hit).map(trial => trial[7]);
	var allDoubleHits = C3T1.concat(C3T2).filter(trial => trial.is_hit).map(trial => trial[7]);
	var allNoCueHits = C1T1.concat(C1T2).filter(trial => trial.is_hit).map(trial => trial[7]);
	
	summary[1][25] = allSpatialHits.length > 0 ? Math.round((allSpatialHits.reduce((a,b) => a+b, 0) / allSpatialHits.length) * 100) / 100 : 0;  // spatial
	summary[1][24] = allCentreHits.length > 0 ? Math.round((allCentreHits.reduce((a,b) => a+b, 0) / allCentreHits.length) * 100) / 100 : 0;  // centre
	summary[1][23] = allDoubleHits.length > 0 ? Math.round((allDoubleHits.reduce((a,b) => a+b, 0) / allDoubleHits.length) * 100) / 100 : 0;  // double
	summary[1][22] = allNoCueHits.length > 0 ? Math.round((allNoCueHits.reduce((a,b) => a+b, 0) / allNoCueHits.length) * 100) / 100 : 0;  // nocue
	
	// pc.all et e.all calculés sur TOUS les essais du test (conforme au script R lignes 166-169)
	// pc.all = (n_hits / n_test_trials) * 100
	// e.all = ((n_test_trials - n_hits) / n_test_trials) * 100
	var n_test_trials = allResults.length;
	var n_hits = allResults.filter(trial => trial.is_hit).length;
	summary[1][20] = n_test_trials > 0 ? Math.round((n_hits / n_test_trials) * 10000) / 100 : 0;  // pc.all
	summary[1][21] = n_test_trials > 0 ? Math.round(((n_test_trials - n_hits) / n_test_trials) * 10000) / 100 : 0;  // e.all
	
	// Effets réseau (conforme au script R lignes 200-203)
	summary[1][19] = Math.round((summary[1][27] - summary[1][26]) * 100) / 100;  // conflict = incong - cong
	summary[1][18] = Math.round((summary[1][24] - summary[1][25]) * 100) / 100;  // orient = centre - spatial
	summary[1][17] = Math.round((summary[1][22] - summary[1][23]) * 100) / 100;  // alert = nocue - double

	return summary;
}

function generateData(userInfo, results) {
	var outputResults = [];
	// The first row is the headings
	outputResults[0] = ['uniqueID', 'StudyNum', 'age', 'sex', 'group', 'targFile', 'Date', 'block', 'trial', 'CueType', 'TargLoc', 'TargDirection', 'Congruency', 'TrialStartTime', 'targetOnTime', 'firstFix', 'Response', 'Correct', 'RT', 'LowRT'];
	destinationRow = 1;												// We've already filled in outputResults[0], so we'll start from 1
	for (sourceTest in results) {			// The test number (should only be 0,1,2)
		for (sourceTrial in results[sourceTest]) {	// The trial within that test (32/test)
			outputResults[destinationRow] = [];
			outputResults[destinationRow][0] = userInfo[0];						//uniqueID
			outputResults[destinationRow][1] = userInfo[4];						//studyNum
			outputResults[destinationRow][2] = userInfo[1];						//age
			outputResults[destinationRow][3] = userInfo[2];						//sex
			outputResults[destinationRow][4] = userInfo[5];						//group
			outputResults[destinationRow][5] = userInfo[6];						//targetType
			outputResults[destinationRow][6] = formatDate(userInfo[7]);			//Date
			outputResults[destinationRow][7] = parseInt(sourceTest);			//block
			outputResults[destinationRow][8] = parseInt(sourceTrial) + 1;		//trial
			outputResults[destinationRow][9] = results[sourceTest][sourceTrial][0];					//CueType
			outputResults[destinationRow][10] = results[sourceTest][sourceTrial][2];				//TargLoc
			outputResults[destinationRow][11] = results[sourceTest][sourceTrial][4];				//TargDirection
			outputResults[destinationRow][12] = results[sourceTest][sourceTrial][1];				//Congruency
			outputResults[destinationRow][13] = formatTime(results[sourceTest][sourceTrial][10]);	//trialStartTime
			outputResults[destinationRow][14] = formatTime(results[sourceTest][sourceTrial][11]);	//targetOnTime
			outputResults[destinationRow][15] = results[sourceTest][sourceTrial][6];				//firstFix
			switch (results[sourceTest][sourceTrial][8]) {	//Response
				case 70:
					outputResults[destinationRow][16] = 'L';
					break;
				case 74:
					outputResults[destinationRow][16] = 'R';
					break;
				default:
					outputResults[destinationRow][16] = 'None';
					break;
			}
			outputResults[destinationRow][17] = isCorrect(results[sourceTest][sourceTrial]);		//Correct
			outputResults[destinationRow][18] = results[sourceTest][sourceTrial][7];				//RT
			if (results[sourceTest][sourceTrial][7] < 100) {										//LowRT
				outputResults[destinationRow][19] = 1;
			} else {
				outputResults[destinationRow][19] = 0;
			}
			destinationRow++;
		}
	}
	return outputResults;	//Returns the total results at a big 2 dimensional array
}

//Array Filtering

//This function checks if an entry is correct or not.  To be correct, the response time must be between 200ms (perseveration threshold) and 1700ms (too-slow threshold) AND the correct key must be pressed.
function isCorrect(recordEntry) {
	if ((recordEntry[7] >= 200) && (recordEntry[7] <= 1700) && (((recordEntry[4] == 'L') && (recordEntry[8] == 70)) || ((recordEntry[4] == 'R') && (recordEntry[8] == 74)))) {
		return 1;
	} else {
		return 0;
	}
}

//Filters an array of trials by correctness, returning only the correct
function onlyCorrect(inputArray) {
	outputArray = [];
	for (i in inputArray) {
		if (isCorrect(inputArray[i])) {
			outputArray.push(inputArray[i]);
		}
	}
	return outputArray;
}

//Returns a simple array of the values at indexOfValues of the given complex array
function valueArray(inputArray, indexOfValues) {
	outputArray = [];
	for (i in inputArray) {
		outputArray.push(inputArray[i][indexOfValues]);
	}
	return outputArray;
}

// Fonction helper pour calculer la pente (régression linéaire simple)
function calculateSlope(values) {
	if (values.length < 2) return null;

	var n = values.length;
	var sumX = 0, sumY = 0, sumXY = 0, sumX2 = 0;

	for (let i = 0; i < n; i++) {
		let x = i + 1; // Numéro de bloc (1, 2, 3, 4)
		let y = values[i];
		sumX += x;
		sumY += y;
		sumXY += x * y;
		sumX2 += x * x;
	}

	// Pente = (n*ΣXY - ΣX*ΣY) / (n*ΣX² - (ΣX)²)
	var slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX);
	return slope;
}
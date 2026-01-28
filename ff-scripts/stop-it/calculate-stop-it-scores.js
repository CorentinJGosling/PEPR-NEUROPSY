// Calcul des indices STOP-IT selon le script R
// Conforme à stopit_scoring.R (lignes 46-169)

function calculateStopItScores(data) {
  // Filtrage des essais : block_i > 0 (exclure practice) ET focus == 'focus' ET Fullscreen == 'yes'
  var dataset_focus = data.filter(row => 
    row.block_i > 0 && 
    row.focus === 'focus' && 
    row.Fullscreen === 'yes'
  );
  
  // Séparer les essais stop et go
  var stopTrials = dataset_focus.filter(row => row.signal === 'yes');
  var goTrials = dataset_focus.filter(row => row.signal === 'no');
  
  var Nstop = stopTrials.length;
  var Ngo = goTrials.length;
  
  // ===== ESSAIS STOP =====
  // presp: probabilité de réponse sur essais stop (1 = répondu, 0 = inhibé)
  var stopWithResponse = stopTrials.filter(row => row.response !== 'undefined' && row.response !== undefined);
  var presp = Nstop > 0 ? stopWithResponse.length / Nstop : 0;
  
  // SSD moyen
  var ssdArray = stopTrials.map(row => row.SSD).filter(ssd => ssd !== undefined && ssd !== null);
  var ssd = ssdArray.length > 0 ? Math.round(ssdArray.reduce((acc, val) => acc + val, 0) / ssdArray.length) : 0;
  
  // usRT: temps de réaction moyen sur essais stop échoués (avec réponse)
  // Remplacer les RT manquants par -250 comme dans le script R (ligne 96)
  var usRTArray = stopWithResponse.map(row => row.rt !== undefined && row.rt !== null ? row.rt : -250);
  var usRT = usRTArray.length > 0 ? Math.round(usRTArray.reduce((acc, val) => acc + val, 0) / usRTArray.length) : 0;
  
  // ===== ESSAIS GO =====
  // Essais Go avec réponse
  var goWithResponse = goTrials.filter(row => row.response !== 'undefined' && row.response !== undefined);
  
  // goRT_all: moyenne de tous les RT Go (remplacer RT manquants par -250)
  var goRTArray_all = goWithResponse.map(row => row.rt !== undefined && row.rt !== null ? row.rt : -250);
  var goRT_all = goRTArray_all.length > 0 ? Math.round(goRTArray_all.reduce((acc, val) => acc + val, 0) / goRTArray_all.length) : 0;
  
  // goRT_sd: écart-type des RT Go (ligne 107)
  // Utiliser la variance d'échantillon (n-1) comme R
  var goRT_mean = goRT_all;
  var n = goRTArray_all.length;
  var goRT_variance = n > 1 ? goRTArray_all.reduce((acc, val) => acc + Math.pow(val - goRT_mean, 2), 0) / (n - 1) : 0;
  var goRT_sd = Math.round(Math.sqrt(goRT_variance));
  
  // Essais Go corrects (ligne 113-114)
  var goCorrectTrials = goTrials.filter(row => row.correct === true || row.correct === 'true' || row.correct === 'TRUE');
  var goRT_correct_array = goCorrectTrials.map(row => row.rt).filter(rt => rt !== undefined && rt !== null);
  var goRT_correct = goRT_correct_array.length > 0 ? 
    Math.round(goRT_correct_array.reduce((acc, val) => acc + val, 0) / goRT_correct_array.length) : 0;
  
  // Taux d'omission Go (pas de réponse) - ligne 116
  var go_omission = Ngo > 0 ? 1 - (goWithResponse.length / Ngo) : 0;
  
  // Taux d'erreur de choix Go (mauvaise réponse parmi ceux qui ont répondu) - ligne 117
  var go_error = goWithResponse.length > 0 ? 1 - (goCorrectTrials.length / goWithResponse.length) : 0;
  
  // Taux de réponses prématurées (RT < 0) - lignes 119-120
  var prematureTrials = goTrials.filter(row => row.rt !== undefined && row.rt !== null && row.rt < 0);
  var go_premature = Ngo > 0 ? prematureTrials.length / Ngo : 0;
  
  // ===== INDICES ADDITIONNELS (lignes 112-140) =====
  
  // goRT_median: Médiane des RT corrects sur essais Go (ligne 115)
  var goRT_median = 0;
  if (goRT_correct_array.length > 0) {
    var sortedCorrectRT = goRT_correct_array.slice().sort((a, b) => a - b);
    var mid = Math.floor(sortedCorrectRT.length / 2);
    if (sortedCorrectRT.length % 2 === 0) {
      goRT_median = Math.round((sortedCorrectRT[mid - 1] + sortedCorrectRT[mid]) / 2);
    } else {
      goRT_median = Math.round(sortedCorrectRT[mid]);
    }
  }
  
  // goRT_cv: Coefficient de variation (CV) = (SD/Mean) × 100 (ligne 117)
  var goRT_cv = 0;
  if (goRT_correct_array.length > 1 && goRT_correct > 0) {
    var correctRT_mean = goRT_correct_array.reduce((acc, val) => acc + val, 0) / goRT_correct_array.length;
    var correctRT_variance = goRT_correct_array.reduce((acc, val) => acc + Math.pow(val - correctRT_mean, 2), 0) / (goRT_correct_array.length - 1);
    var correctRT_sd = Math.sqrt(correctRT_variance);
    goRT_cv = Math.round((correctRT_sd / correctRT_mean) * 100 * 100) / 100;
  }
  
  // go_lapses: % d'essais Go corrects avec RT > 1000ms (ligne 119)
  var go_lapses = 0;
  if (goRT_correct_array.length > 0) {
    var lapsesCount = goRT_correct_array.filter(rt => rt > 1000).length;
    go_lapses = Math.round((lapsesCount / goRT_correct_array.length) * 100 * 100) / 100;
  }
  
  // go_perseverations: Nombre d'essais Go avec RT < 200ms (ligne 121)
  var go_perseverations = goWithResponse.filter(row => row.rt !== undefined && row.rt !== null && row.rt < 200).length;
  
  // go_outliers: Nombre d'essais Go corrects avec RT > M+3SD ou < M-3SD (ligne 125)
  var go_outliers = 0;
  if (goRT_correct_array.length > 1) {
    var m_rt = goRT_correct_array.reduce((acc, val) => acc + val, 0) / goRT_correct_array.length;
    var variance_rt = goRT_correct_array.reduce((acc, val) => acc + Math.pow(val - m_rt, 2), 0) / (goRT_correct_array.length - 1);
    var s_rt = Math.sqrt(variance_rt);
    var upper_bound = m_rt + 3 * s_rt;
    var lower_bound = m_rt - 3 * s_rt;
    go_outliers = goRT_correct_array.filter(rt => rt > upper_bound || rt < lower_bound).length;
  }
  
  // Slope_RT et Slope_SE: Pentes de régression par bloc (lignes 128-140)
  var Slope_RT = 0;
  var Slope_SE = 0;
  
  // Calculer les statistiques par bloc pour les essais Go corrects
  var blockStats = {};
  goCorrectTrials.forEach(trial => {
    var block = trial.block_i;
    if (!blockStats[block]) {
      blockStats[block] = { rts: [], block_i: block };
    }
    if (trial.rt !== undefined && trial.rt !== null) {
      blockStats[block].rts.push(trial.rt);
    }
  });
  
  // Calculer mean_rt et se_rt par bloc
  var blockData = Object.keys(blockStats).map(block => {
    var rts = blockStats[block].rts;
    var n = rts.length;
    var mean_rt = n > 0 ? rts.reduce((acc, val) => acc + val, 0) / n : 0;
    var variance = n > 1 ? rts.reduce((acc, val) => acc + Math.pow(val - mean_rt, 2), 0) / (n - 1) : 0;
    var sd = Math.sqrt(variance);
    var se_rt = n > 0 ? sd / Math.sqrt(n) : 0;
    return {
      block_i: parseInt(block),
      mean_rt: mean_rt,
      se_rt: se_rt
    };
  }).sort((a, b) => a.block_i - b.block_i);
  
  // Régression linéaire simple: y = a + b*x
  function linearRegression(data, xKey, yKey) {
    var n = data.length;
    if (n < 2) return 0;
    
    var sum_x = 0, sum_y = 0, sum_xy = 0, sum_xx = 0;
    data.forEach(point => {
      var x = point[xKey];
      var y = point[yKey];
      sum_x += x;
      sum_y += y;
      sum_xy += x * y;
      sum_xx += x * x;
    });
    
    var slope = (n * sum_xy - sum_x * sum_y) / (n * sum_xx - sum_x * sum_x);
    return Math.round(slope * 100) / 100;
  }
  
  if (blockData.length >= 2) {
    Slope_RT = linearRegression(blockData, 'block_i', 'mean_rt');
    Slope_SE = linearRegression(blockData, 'block_i', 'se_rt');
  }
  
  // ===== CALCUL DU SSRT (MÉTHODE DU QUANTILE / INTEGRATION METHOD) =====
  // Lignes 109-123 du script R
  
  // 1. Trouver le RT max parmi les essais Go avec réponse (ligne 109)
  var goRTValues = goWithResponse.map(row => row.rt).filter(rt => rt !== undefined && rt !== null);
  var goRT_max = goRTValues.length > 0 ? Math.max(...goRTValues) : 0;
  
  // 2. Créer goRT_adj: remplacer les omissions (pas de réponse) par goRT_max (ligne 110)
  var goRT_adj = goTrials.map(row => {
    if (row.response === 'undefined' || row.response === undefined) {
      return goRT_max;
    }
    return row.rt !== undefined && row.rt !== null ? row.rt : goRT_max;
  });
  
  // 3. Trier goRT_adj
  goRT_adj.sort((a, b) => a - b);
  
  // 4. Calculer le n-ième percentile basé sur presp (ligne 111)
  // Méthode quantile type 6 de R: p[k] = k / (n + 1)
  function quantile(sortedArray, prob) {
    if (sortedArray.length === 0) return 0;
    var n = sortedArray.length;
    var index = prob * (n + 1) - 1; // Type 6: p[k] = k/(n+1), donc k = p*(n+1)
    
    if (index < 0) return sortedArray[0];
    if (index >= n - 1) return sortedArray[n - 1];
    
    var lower = Math.floor(index);
    var upper = Math.ceil(index);
    var weight = index - lower;
    
    return sortedArray[lower] + weight * (sortedArray[upper] - sortedArray[lower]);
  }
  
  var nth = Math.round(quantile(goRT_adj, presp));
  
  // 5. SSRT = nth percentile - SSD moyen (ligne 123)
  var ssrt = nth - ssd;
  
  // ===== SCORES FINAUX (lignes 142-163) =====
  // Arrondissements conformes au script R:
  // - presp, go_omission, go_error, go_premature: pas d'arrondi (valeurs brutes)
  // - ssd, usRT, goRT_all, goRT_correct, goRT_sd, goRT_median: arrondi à l'entier (déjà fait)
  // - goRT_cv, go_lapses, Slope_RT, Slope_SE: arrondi à 2 décimales (déjà fait)
  return {
    presp: presp,
    ssd: ssd,
    ssrt: ssrt,
    usRT: usRT,
    goRT_all: goRT_all,
    goRT_correct: goRT_correct,
    goRT_sd: goRT_sd,
    go_omission: go_omission,
    go_error: go_error,
    go_premature: go_premature,
    Nstop: Nstop,
    Ngo: Ngo,
    goRT_cv: goRT_cv,
    go_lapses: go_lapses,
    Slope_RT: Slope_RT,
    Slope_SE: Slope_SE,
    goRT_median: goRT_median,
    go_perseverations: go_perseverations,
    go_outliers: go_outliers,
    // Champs de compatibilité (calculés à partir des indices ci-dessus)
    successOnStopTrialsRate: 1 - presp,
    accuracyOnGoTrials: 1 - go_error
  };
}

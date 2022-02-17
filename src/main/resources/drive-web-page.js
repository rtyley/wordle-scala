function stateFor(feedbackChar) {
    switch (feedbackChar) {
        case 'B':
            return 'absent';
        case 'Y':
            return 'present';
        case 'G':
            return 'correct';
    }
}

function rowAt(index) {
    return document.body.getElementsByTagName("game-app")[0].shadowRoot.querySelectorAll("game-row")[index];
}

function setWord(rowIndex, word) {
    rowAt(rowIndex).setAttribute('letters',word);
}

function clear() {
  for (var i = 0; i < 6; i++) {
      setWord(i, "");
  }
}

function setEvidence(rowIndex, word, feedback) {
    setWord(rowIndex, word);
    const row = rowAt(rowIndex)
    for (var i = 0; i < feedback.length; i++) {
        feedback[i]
        row.shadowRoot.querySelectorAll("game-tile")[i].shadowRoot.querySelector(".tile").dataset.state = stateFor(feedback[i]);
    }
}
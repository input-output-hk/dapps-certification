var baseUrl = 'https://testing.dapps.iog.io/';

const toggleLoader = (show) => {
    if (show) {
        document.getElementById('start').classList.add('disabled');
        document.getElementById('overlay').classList.remove('hidden');
    } else {
        document.getElementById('start').classList.remove('disabled');
        document.getElementById('overlay').classList.add('disabled');
    }
}

async function postData(url = '', data = '') {
    var myHeaders = new Headers();
    myHeaders.append("Content-Type", "text/plain;charset=utf-8");
    myHeaders.append("Accept", "text/plain;charset=utf-8");

    var requestOptions = {
        method: 'POST',
        headers: myHeaders,
        body: data,
        redirect: 'follow'
    };
    
    const api = await fetch(url, requestOptions)
        .catch(error => {
            console.log('error', error)
        });
    return api;
}

async function fetchData(method, url, data = '') {
    var myHeaders = new Headers();
    myHeaders.append("Content-Type", "application/json");

    var requestOptions = {
        method: method,
        mode: 'no-cors',
        headers: myHeaders,
        redirect: 'follow'
    };
    if (data) {
        requestOptions['body'] = data;
    }
    const api = await fetch(url, requestOptions)
        .catch(error => {
            console.log('error', error)
        });
    return api;
}

var validateForm = function() {
    valid = true;
    const inputs = document.getElementById('searchForm').querySelectorAll('div.input')
    Array.from(inputs).forEach(input => {
        const currentVal = input.children[1].value
        if (!currentVal || !currentVal.trim()) {
            valid = false;
            input.classList.add('error')
        } else {
            input.classList.remove('error')
        }
    })
    // apply any more character validations as needed
    return valid;
}

function startCertification(evt) {
    
    evt.stopImmediatePropagation();
    evt.preventDefault();
    
    var clearForm = function() {
        document.getElementById('searchForm').reset();
    }

    if (!validateForm())
        return; // do nothing

    toggleLoader(true)

    const username = document.getElementById('username').value,
        repoName = document.getElementById('repoName').value,
        branch = document.getElementById('branch').value;

    // if inputElm.value is not-empty && has changed, trigger post api
    const dappRef = 'github:' + [username, repoName, branch].join('/')

    postData(baseUrl + 'run', dappRef)
    .then((response) => response.text())
    .then(data => {
        retrieveRunStatus(data);
    });
}

function fetchSVG(type = 'outline') {
    return fetch('assets/images/' + type + '.svg')
        .then(r => r.text())
        .catch(console.error.bind(console));
}

function initialStatusTimelineDisplay(type = 'outline') {
    var imgs = document.getElementById('statusTimeline').querySelectorAll('span.image')
    imgs.forEach(img => {
        fetchSVG(type).then(res => {
            img.innerHTML = res;
        })
    })
}

function updateTimelineDisplay(status, type) {
    var img = document.querySelector('li[data-value="' + status + '"]').querySelector('span.image')
    fetchSVG(type).then(res => {
        img.innerHTML = res;
    })
}

function updateEntireTimeline(currentStatus, currentType) {
    const statusSet = ['queued', 'preparing', 'building', 'certifying', 'finished']

    const statusValues = {
        queued: 'outline',
        preparing: 'outline',
        building: 'outline',
        certifying: 'outline',
        finished: 'outline'
    }

    const foundAt = statusSet.indexOf(currentStatus)
    if (foundAt == -1) {
        return; // do nothing
    }
    statusSet.forEach((status, i) => {
        if (foundAt < i) {
            statusValues[status] = 'passed'
            updateTimelineDisplay(status, 'passed')
        } else if (foundAt === i && status === currentStatus) {
            statusValues[status] = currentType
            updateTimelineDisplay(currentStatus, currentType)
            return; // to break the loop
        }
    })
    
}

function retrieveRunStatus(uuid = '') {
    if (!uuid) 
        return;

    document.getElementById('resultContainer').classList.add('visible')
    
    initialStatusTimelineDisplay()

    const triggerFetchRunStatus = async () => {
        const url = baseUrl + 'run/' + uuid;
        await fetchData('GET', url).then(response => {

            const res = response.json()
            const status = res.status;  
        
            if (status !== 'finished') {
                updateEntireTimeline(status, 'running')

                const timeOffset = 60*1000,
                    refetchMins = 2;
                const timeout = setTimeout(() => {
                    clearTimeout(timeout);
                    triggerFetchRunStatus()
                }, refetchMins * timeOffset)
            } else {
                updateEntireTimeline(status, 'passed')
                processFinishedJson(res.result);
                toggleLoader(false)
            }
        })
    }
    triggerFetchRunStatus();
}

function processFinishedJson(result) {
    for (var key in result) {
        if (key.endsWith('Result') && result[key] && result[key].tag === 'Failure') {
            var template = '<span>' + result[key].reason + '</span>'
            result[key].failingTestCase.forEach(val => {
                template += '<span>Test Case: ' + val + '</span>'
            })
            template += '<span>' + result[key].output + '</span>';

            const failedCase = document.createElement('div')
            failedCase.innerHTML = template;
            failedCase.classList.add('failure')
            document.getElementById('logContainer').append(failedCase)
        }
    }
}


let activeInput = false;

const activateInput = (evt) => {
    activeInput = true;
    const inputWrapper = evt.currentTarget;
    inputWrapper.classList.add('active');
    inputWrapper.children[1].focus()
}

const deactivateInputIfEmpty = () => {
    const actives = document.querySelectorAll('div.input.active')
    Array.from(actives).forEach(elm => {
        if (!elm.children[1].value || !elm.children[1].value.trim()) {
            activeInput = false;
            elm.classList.remove('active')
        }
    })
}

const wrappers = document.querySelectorAll('div.input')
Array.from(wrappers).forEach(elm => {
    elm.addEventListener('click', (evt) => {
        evt.stopImmediatePropagation();
        if (activateInput) {
            deactivateInputIfEmpty()
        }
        if(!elm.className.includes('active')){
            activateInput(evt)
        }
    })
})

document.getElementById('start').addEventListener('click', (evt) => {
    startCertification(evt);
})

document.addEventListener('click', (evt) => {
    if (evt.currentTarget.className !== 'input' && activeInput) {
        deactivateInputIfEmpty()
    }
})

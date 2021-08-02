import { h, Component, render } from 'preact'
import {
  createStateQueryClient,
  StateQuery,
  Schema,
  createInteractionContext
} from '@cardano-ogmios/client'

import AstroKitten from './assets/img/astro-kitten.svg'
import './style.css'

interface AppState {
  address: Schema.Address,
  utxo?: Schema.Utxo,
  assets: string[],
  protocolParameters: Schema.ProtocolParametersShelley | Schema.ProtocolParametersAlonzo
}

class App extends Component<{}, AppState> {
  client : StateQuery.StateQueryClient
  watchUtxo : ReturnType<typeof setInterval>

  constructor () {
    super()
    this.setState({
      assets: [],
      address: 'addr_test1qplaag066gege5ly8ym9lgpcrpha2g424arrkfpf0uztezkm7g22jrj643mmu6cc7vct03ng7pf7sy5rls5y4d6xl2aq5m3nx9'
    })
  }

  componentDidMount () {
    createInteractionContext(
      err => console.error(err),
      () => console.error('Connection closed.')
    )
      .then(createStateQueryClient)
      .then(client => {
        this.client = client

        // install balance watcher
        this.watchUtxo = setInterval(() => {
          if (this.state.address) {
            client.utxo([this.state.address])
              .then(utxo => this.setState({ utxo }))
          }
        }, 2500)

        // fetch protocol parameters
        this.client.currentProtocolParameters()
          .then(protocolParameters => {
            this.setState({ protocolParameters })
          })
      })
  }

  componentWillUnmount () {
    if (this.watchUtxo) { clearInterval(this.watchUtxo) }
  }

  render (_ : {}, state : AppState) {
    const assets = state.assets.length === 0
      ? (<div className="no-assets">
          <img src={AstroKitten}/>
          <span>It doesn't look like you own any assets...</span>
        </div>)
      : 'TODO'

    const balance = formatBalance(state.utxo)

    return (<div>
      <div className={'header'}>
        <h2>Wallet</h2>
        <h1 className={'ada'}>{balance}</h1>
      </div>
      <div className={'main'}>
        <h4>Assets</h4>
        {assets}
      </div>
      <div className={'footer'}>
        <button>
          <i class="fa fa-share-square-o" aria-hidden="true"></i><br/>
          send
        </button>
        <button>
          <i class="fa fa-qrcode" aria-hidden="true"></i><br/>
          receive
        </button>
      </div>

    </div>)
  }
}

function getAdaBalance (utxo : Schema.Utxo) : Schema.Lovelace {
  return utxo.reduce((total, [_, out]) => total + out.value.coins, 0) / 1e6
}

function formatBalance (utxo? : Schema.Utxo) : string {
  return utxo === undefined ? '...' : getAdaBalance(utxo).toFixed(6)
}

render(<App />, document.querySelector('#app'))

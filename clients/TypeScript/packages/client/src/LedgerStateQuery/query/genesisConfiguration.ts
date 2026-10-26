/* eslint no-redeclare: "off" */
import { InteractionContext, Method, MethodOptions } from '../../Connection'
import {
  Ogmios,
  GenesisAlonzo,
  GenesisByron,
  GenesisConway,
  GenesisDijkstra,
  GenesisShelley,
  EraWithGenesis
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryNetworkGenesisConfiguration']
type Response = Ogmios['QueryNetworkGenesisConfigurationResponse']

/**
 * Get the genesis configuration of a specific era with configuration.
 *
 * @category LedgerStateQuery
 */
export function genesisConfiguration(context: InteractionContext, era: 'byron', options?: MethodOptions): Promise<GenesisByron>
export function genesisConfiguration(context: InteractionContext, era: 'shelley', options?: MethodOptions): Promise<GenesisShelley>
export function genesisConfiguration(context: InteractionContext, era: 'alonzo', options?: MethodOptions): Promise<GenesisAlonzo>
export function genesisConfiguration(context: InteractionContext, era: 'conway', options?: MethodOptions): Promise<GenesisConway>
export function genesisConfiguration(context: InteractionContext, era: 'dijkstra', options?: MethodOptions): Promise<GenesisDijkstra>
export function genesisConfiguration (
  context: InteractionContext,
  era: EraWithGenesis,
  options?: MethodOptions
): Promise<GenesisByron | GenesisShelley | GenesisAlonzo | GenesisConway | GenesisDijkstra> {
  return Method<Request, Response, GenesisByron | GenesisShelley | GenesisAlonzo | GenesisConway | GenesisDijkstra>(
    {
      method: 'queryNetwork/genesisConfiguration',
      params: { era }
    },
    {
      handler (response, resolve, reject) {
        if (era === 'byron' && isQueryNetworkGenesisConfigurationByron(response)) {
          resolve(response.result as GenesisByron)
        } else if (era === 'shelley' && isQueryNetworkGenesisConfigurationShelley(response)) {
          resolve(response.result as GenesisShelley)
        } else if (era === 'alonzo' && isQueryNetworkGenesisConfigurationAlonzo(response)) {
          resolve(response.result as GenesisAlonzo)
        } else if (era === 'conway' && isQueryNetworkGenesisConfigurationConway(response)) {
          resolve(response.result as GenesisConway)
        } else if (era === 'dijkstra' && isQueryNetworkGenesisConfigurationDijkstra(response)) {
          resolve(response.result as GenesisDijkstra)
        } else {
          reject(response)
        }
      }
    },
    context,
    options)
}

/**
 * @internal
 */
export function isQueryNetworkGenesisConfigurationByron (response: any): response is GenesisByron {
  return ((response as Response)?.result as GenesisByron)?.era === 'byron'
}

/**
 * @internal
 */
export function isQueryNetworkGenesisConfigurationShelley (response: any): response is GenesisShelley {
  return ((response as Response)?.result as GenesisShelley)?.era === 'shelley'
}

/**
 * @internal
 */
export function isQueryNetworkGenesisConfigurationAlonzo (response: any): response is GenesisAlonzo {
  return ((response as Response)?.result as GenesisAlonzo)?.era === 'alonzo'
}

/**
 * @internal
 */
export function isQueryNetworkGenesisConfigurationConway (response: any): response is GenesisConway {
  return ((response as Response)?.result as GenesisConway)?.era === 'conway'
}

/**
 * @internal
 */
export function isQueryNetworkGenesisConfigurationDijkstra (response: any): response is GenesisDijkstra {
  return ((response as Response)?.result as GenesisDijkstra)?.era === 'dijkstra'
}

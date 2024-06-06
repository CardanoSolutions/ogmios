/* eslint no-redeclare: "off" */
import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  GenesisAlonzo,
  GenesisByron,
  GenesisConway,
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
export function genesisConfiguration(context: InteractionContext, era: 'byron'): Promise<GenesisByron>
export function genesisConfiguration(context: InteractionContext, era: 'shelley'): Promise<GenesisShelley>
export function genesisConfiguration(context: InteractionContext, era: 'alonzo'): Promise<GenesisAlonzo>
export function genesisConfiguration(context: InteractionContext, era: 'conway'): Promise<GenesisConway>
export function genesisConfiguration (
  context: InteractionContext,
  era: EraWithGenesis
): Promise<GenesisByron | GenesisShelley | GenesisAlonzo | GenesisConway> {
  return Method<Request, Response, GenesisByron | GenesisShelley | GenesisAlonzo | GenesisConway>(
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
        } else {
          reject(response)
        }
      }
    },
    context)
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

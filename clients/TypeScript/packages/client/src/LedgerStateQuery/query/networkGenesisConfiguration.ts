import { InteractionContext, Method } from '../../Connection'
import {
  Ogmios,
  GenesisAlonzo,
  GenesisByron,
  GenesisShelley,
  EraWithGenesis,
} from '@cardano-ogmios/schema'

type Request = Ogmios['QueryNetworkGenesisConfiguration']
type Response = Ogmios['QueryNetworkGenesisConfigurationResponse']

/**
 * Get the genesis configuration of a specific era with configuration.
 *
 * @category LedgerStateQuery
 */
export function networkGenesisConfiguration(context: InteractionContext, era: "byron"): Promise<GenesisByron>
export function networkGenesisConfiguration(context: InteractionContext, era: "shelley"): Promise<GenesisShelley>
export function networkGenesisConfiguration(context: InteractionContext, era: "alonzo"): Promise<GenesisAlonzo>
export function networkGenesisConfiguration(
  context: InteractionContext,
  era: EraWithGenesis,
): Promise<GenesisByron | GenesisShelley | GenesisAlonzo> {
  return Method<Request, Response, GenesisByron | GenesisShelley | GenesisAlonzo>(
    {
      method: 'queryNetwork/genesisConfiguration',
      params: { era }
    },
    {
      handler(response, resolve, reject) {
        if (era === 'byron' && isQueryNetworkGenesisConfigurationByron(response)) {
          resolve((response.result.genesisConfiguration as { 'byron': GenesisByron }).byron)
        } else if (era === 'shelley' && isQueryNetworkGenesisConfigurationShelley(response)) {
          resolve((response.result.genesisConfiguration as { 'shelley': GenesisShelley }).shelley)
        } else if (era === 'alonzo' && isQueryNetworkGenesisConfigurationAlonzo(response)) {
          resolve((response.result.genesisConfiguration as { 'alonzo': GenesisAlonzo }).alonzo)
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
export function isQueryNetworkGenesisConfigurationByron(response: any): response is GenesisByron {
  const genesisConfiguration = (response as Response)?.result?.genesisConfiguration
  return 'byron' in genesisConfiguration
}

/**
 * @internal
 */
export function isQueryNetworkGenesisConfigurationShelley(response: any): response is GenesisShelley {
  const genesisConfiguration = (response as Response)?.result?.genesisConfiguration
  return 'shelley' in genesisConfiguration
}

/**
 * @internal
 */
export function isQueryNetworkGenesisConfigurationAlonzo(response: any): response is GenesisAlonzo {
  const genesisConfiguration = (response as Response)?.result?.genesisConfiguration
  return 'alonzo' in genesisConfiguration
}

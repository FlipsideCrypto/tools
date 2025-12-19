# Plan: Consolidate query_primitive into query_execution Domain

## Overview

This plan outlines the consolidation of the `query_primitive` domain into the `query_execution` domain, resulting in a single unified domain model for query management and execution via the V3 system.

---

## Current State (Assumptions)

### query_primitive Domain
Contains:
- `slug` - Unique identifier for the query primitive
- `description` - Human-readable description of what the query does
- `parameters` - Input parameters/schema for the query
- (Other fields to be deprecated)

### query_execution Domain
Contains:
- Execution state/status
- Results/output
- Timing/performance data
- (Other execution-related fields)

---

## Target State: Unified query_execution Domain

### New Domain Model Schema

```typescript
interface QueryExecution {
  // === Fields migrated from query_primitive ===
  slug: string;                    // Unique identifier (from query_primitive)
  description: string;             // Human-readable description (from query_primitive)
  parameters: QueryParameters;     // Input parameters schema (from query_primitive)

  // === Execution fields (existing) ===
  id: string;                      // Unique execution ID
  status: ExecutionStatus;         // 'pending' | 'running' | 'completed' | 'failed'
  created_at: Date;
  started_at?: Date;
  completed_at?: Date;

  // === Results ===
  result?: QueryResult;
  error?: QueryError;

  // === V3 System Integration ===
  v3_query_id?: string;           // Reference to V3 query system
  v3_execution_id?: string;       // V3 execution tracking
}

interface QueryParameters {
  schema: JSONSchema;              // Parameter definitions
  values: Record<string, any>;     // Actual parameter values
}

type ExecutionStatus = 'pending' | 'running' | 'completed' | 'failed' | 'cancelled';
```

---

## Migration Steps

### Phase 1: Schema Consolidation

1. **Add query_primitive fields to query_execution model**
   - Add `slug`, `description`, `parameters` fields to query_execution schema
   - Ensure backward compatibility during transition

2. **Create migration script**
   - Copy `slug`, `description`, `parameters` from query_primitive records
   - Link existing query_execution records to their corresponding primitive data
   - Validate data integrity

3. **Update validation**
   - Add validation rules for the new fields
   - Ensure `slug` uniqueness constraints

### Phase 2: New V3 Tools Implementation

Create new tools for V3 query system integration:

| Tool Name | Description | Inputs | Outputs |
|-----------|-------------|--------|---------|
| `create_query_execution` | Create a new query execution with V3 | slug, parameters | execution_id, status |
| `execute_query_v3` | Run a query through V3 system | execution_id OR (slug + parameters) | execution_id, async status |
| `get_query_execution_status` | Check execution status | execution_id | status, progress, timestamps |
| `get_query_execution_result` | Retrieve completed results | execution_id | result data, metadata |
| `cancel_query_execution` | Cancel a running execution | execution_id | success boolean |
| `list_query_executions` | List executions with filters | filters (status, slug, date range) | paginated executions |

#### Tool Specifications

**create_query_execution**
```typescript
interface CreateQueryExecutionInput {
  slug: string;           // Query identifier
  description?: string;   // Optional override
  parameters: Record<string, any>;
}

interface CreateQueryExecutionOutput {
  execution_id: string;
  status: ExecutionStatus;
  created_at: Date;
}
```

**execute_query_v3**
```typescript
interface ExecuteQueryV3Input {
  execution_id?: string;  // Use existing execution
  // OR create new:
  slug?: string;
  parameters?: Record<string, any>;
  async?: boolean;        // Default true for long-running
}

interface ExecuteQueryV3Output {
  execution_id: string;
  status: ExecutionStatus;
  v3_execution_id: string;
  // If sync mode and completed:
  result?: QueryResult;
}
```

**get_query_execution_result**
```typescript
interface GetQueryResultInput {
  execution_id: string;
  format?: 'json' | 'csv' | 'arrow';
  pagination?: {
    offset: number;
    limit: number;
  };
}

interface GetQueryResultOutput {
  data: any[];
  metadata: {
    row_count: number;
    column_definitions: ColumnDef[];
    execution_time_ms: number;
  };
  pagination?: {
    has_more: boolean;
    next_offset: number;
  };
}
```

### Phase 3: Deprecate query_primitive

1. **Mark query_primitive as deprecated**
   - Add deprecation warnings to any query_primitive tools/endpoints
   - Document migration path in changelog

2. **Update all consumers**
   - Identify all services/tools using query_primitive
   - Update to use consolidated query_execution domain
   - Update API endpoints if applicable

3. **Remove query_primitive**
   - After transition period, remove query_primitive model
   - Clean up database tables/collections
   - Remove deprecated code

---

## V3 System Integration Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Client/Agent Layer                       │
│  (Tools: create_query_execution, execute_query_v3, etc.)    │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│              Unified query_execution Domain                  │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  QueryExecution                                      │   │
│  │  - slug, description, parameters (from primitive)    │   │
│  │  - id, status, timestamps (execution tracking)       │   │
│  │  - v3_query_id, v3_execution_id (V3 integration)    │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                    V3 Query System                           │
│  - Query compilation                                         │
│  - Execution orchestration                                   │
│  - Result caching/retrieval                                  │
└─────────────────────────────────────────────────────────────┘
```

---

## Files to Modify (Template)

Assuming a typical domain-driven structure:

```
src/domains/
├── query_execution/
│   ├── models/
│   │   └── query_execution.ts    # UPDATE: Add slug, description, parameters
│   ├── tools/
│   │   ├── create_query_execution.ts    # NEW
│   │   ├── execute_query_v3.ts          # NEW
│   │   ├── get_query_execution_status.ts # NEW or UPDATE
│   │   ├── get_query_execution_result.ts # NEW or UPDATE
│   │   ├── cancel_query_execution.ts    # NEW
│   │   └── list_query_executions.ts     # NEW or UPDATE
│   ├── services/
│   │   └── v3_integration_service.ts    # NEW: V3 system integration
│   └── index.ts                         # UPDATE: Export new tools
│
├── query_primitive/                     # TO BE REMOVED
│   └── ... (deprecated)
```

---

## Migration Checklist

- [ ] Design finalized schema for unified query_execution model
- [ ] Create database migration for new fields
- [ ] Implement data migration from query_primitive
- [ ] Implement new V3 tools
- [ ] Update existing consumers of query_primitive
- [ ] Add deprecation notices
- [ ] Integration testing with V3 system
- [ ] Performance testing
- [ ] Documentation updates
- [ ] Remove query_primitive domain
- [ ] Clean up deprecated code/tables

---

## Open Questions

1. **V3 System Details**: What is the exact V3 API interface for query execution?
2. **Async Handling**: How should long-running queries be handled? Webhooks? Polling?
3. **Caching**: Should results be cached in the query_execution record or fetched fresh from V3?
4. **Permissions**: Are there access control considerations for query executions?
5. **Rate Limiting**: Should the new tools have rate limiting built in?

---

## Notes

- This plan assumes the domains exist in a separate repository from `/home/user/tools`
- The actual implementation will need to be adapted based on the existing codebase structure
- V3 integration specifics will need to be filled in based on the V3 system documentation
